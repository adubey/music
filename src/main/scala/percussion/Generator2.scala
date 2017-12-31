package ca.dubey.music.percussion

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.SequencePlayer
import ca.dubey.music.midi.TrackBuilder
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.NoteEvent
import ca.dubey.music.midi.event.NoteOn
import ca.dubey.music.midi.event.NoteOff
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import cc.mallet.classify.MaxEnt
import cc.mallet.types.Alphabet
import cc.mallet.types.Label
import cc.mallet.types.LabelAlphabet
import cc.mallet.types.FeatureVector
import cc.mallet.types.Instance
import collection.mutable.ListBuffer
import collection.mutable.Queue
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.FileInputStream
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.Track

object Generator2 extends App {
  val modelFilename = args(0)
  val TICKS_PER_BEAT = 24
  val sequencer = MidiSystem.getSequencer
  sequencer.open

  val ois = new ObjectInputStream (new FileInputStream(modelFilename));
  val classifier = ois.readObject.asInstanceOf[MaxEnt]
  ois.close

  val builder = new Generator2(TICKS_PER_BEAT, 960, classifier)
  builder.buildFromMotifs
  builder.play(sequencer)

  System.exit(0)
}

class Generator2(
    val ticksPerBeat : Int,
    val beatsPerMinute : Int,
    val classifier : MaxEnt)
    extends SequencePlayer with TrackBuilder with MotifSampler {

  protected val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  protected val t = sequence.createTrack
  private val dataAlphabet : LabelAlphabet = classifier.getAlphabet.asInstanceOf[LabelAlphabet]
  private val targetAlphabet : LabelAlphabet = classifier.getLabelAlphabet.asInstanceOf[LabelAlphabet]

  def sampleNextNote(r : util.Random, song : Analyzer.Song.SongAnalyzer) : Option[Label] = {
      val time : Long = 0
      for (key <- Data.keyLow to Data.keyHigh) {
        for (delta -> Data.deltaQuantizer) {
          val noteOn = NoteOn(time + delta, key, 60)
          val noteOff = NoteOff(time + delta, key)
        }
      }
    }
    val output = song.analyzeWithoutAdvancing
    val vector = new FeatureVector(dataAlphabet, q.map((x:Label) => x.getIndex).toArray)
    val instance = new Instance(vector, null, "", "")
    val classification = classifier.classify(instance)
    val labeling = classification.getLabeling
    var p = 0D
    val targetProb = r.nextDouble
    for (j <- 0 until targetAlphabet.size) {
      p += labeling.getValueAtRank(j)
      val label = labeling.getLabelAtRank(j)
      // val string = dataAlphabet.lookupObject(label.getIndex).asInstanceOf[String]
      val s1 = label.getEntry.asInstanceOf[String]
      val s2 = dataAlphabet.lookupObject(label.getIndex).asInstanceOf[String]
      if (p >= targetProb) {
        return Some(label)
      }
    }
    return Some(labeling.getLabelAtRank(dataAlphabet.size - 1))

    return Option.empty[Label]
  }


  def buildFromMotifs = {
    val motifs = Motif.build(this)
    channel = 9
    addNameEvent(t, "some track")
    t.add(TempoEvent(beatsPerMinute).toMidiEvent)
    // motifs.realize((note:NoteOn) => addNoteOn(t, note.key, note.tick, note.velocity))
  }

  def sampleMotif(numMeasures : Int, prev : Option[BaseMotif] = None)  : BaseMotif = {
    val analyzer = new Analyzer
    channel = 9
    var time = 0L
    val q = new Queue[Label]
    val r = new util.Random
    val maxTime = ticksPerBeat * 4 * numMeasures - ticksPerBeat
    val motif = Array.newBuilder[NoteEvent]

    for (p <- prev) {
      val windowStart = p.events.size - q.size
      var offset = 0L
      // This has a timing bug if p.size > q.size
      for (i <- math.max(0, windowStart - 1) until q.size) {
        val event = p.events(i)
        // q += dataAlphabet.lookupLabel(Data.encode(dataAlphabet, (event.tick - offset).toInt, event.key, event.velocity))
        q.dequeue
        offset = event.tick
      }
    }

    while(time < maxTime) {
      sampleNextNote(r, q) match {
        case None =>
          // Try again
          return sampleMotif(numMeasures)
          
        case Some(label) =>
          val (tick, note, velocity) = Data.decode(label)
          if (tick == 0 && note == 0 && velocity == 0) {
            printf("Stopped early: %s\n", label)
            // Try again
            return sampleMotif(numMeasures)
          }
          time += Data.unnormalizeTick(tick, ticksPerBeat)
          motif += NoteOn(time, note, velocity)
          q += label
          q.dequeue
      }
    }

    return BaseMotif(motif.result)
  }


  def build : Track = {
    channel = 9
    addNameEvent(t, "some track")
    val q = new Queue[Label]
    val r = new util.Random
    var time = 0L


    for (i <- 1 to 500) {
      sampleNextNote(r, q) match {
        case None => return t
        case Some(label) =>
          val (tick, note, velocity) = Data.decode(label)
          if (tick == 0 && note == 0 && velocity == 0) {
            printf("Stopped early: %s\n", label)
            return t
          }
          time += Data.unnormalizeTick(tick, ticksPerBeat)
          addNoteOn(t, note, time, velocity)
          q += label
          q.dequeue
      }

    }

    return t
  }
}
