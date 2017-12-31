package ca.dubey.music.percussion

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.SequencePlayer
import ca.dubey.music.midi.TrackBuilder
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import ca.dubey.music.midi.event.NoteEvent
import ca.dubey.music.midi.event.NoteOff
import ca.dubey.music.midi.event.NoteOn
import ca.dubey.music.midi.event.Skip
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

object Generator extends App {
  val modelFilename = args(0)
  val TICKS_PER_BEAT = 960

  val sequencer = MidiSystem.getSequencer
  sequencer.open

  val ois = new ObjectInputStream (new FileInputStream(modelFilename));
  val classifier = ois.readObject.asInstanceOf[MaxEnt]
  ois.close

  val builder = new Generator(TICKS_PER_BEAT, 200, classifier)
  builder.buildFromMotifs

  printf("Playing\n")
  builder.play(sequencer)

  System.exit(0)
}

class Generator(
    val ticksPerBeat : Int,
    val beatsPerMinute : Int,
    val classifier : MaxEnt)
    extends SequencePlayer with TrackBuilder with MotifSampler {

  var maxLength = 4
  protected val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  protected val t = sequence.createTrack
  private val alphabet : LabelAlphabet = classifier.getAlphabet.asInstanceOf[LabelAlphabet]
  private val targetAlphabet : LabelAlphabet = classifier.getLabelAlphabet.asInstanceOf[LabelAlphabet]

  def sampleNextNote(r : util.Random, h : History) : Option[Label] = {
    val instance = new Instance(h.featureVector, null, "", "")
    val classification = classifier.classify(instance)
    val labeling = classification.getLabeling
    var p = 0D
    val targetProb = r.nextDouble
    for (j <- 0 until alphabet.size) {
      p += labeling.getValueAtRank(j)
      val label = labeling.getLabelAtRank(j)
      val string = targetAlphabet.lookupObject(label.getIndex).asInstanceOf[String]
      val s1 = label.getEntry.asInstanceOf[String]
      val s2 = alphabet.lookupObject(label.getIndex).asInstanceOf[String]
      if (p >= targetProb) {
        return Some(label)
      }
    }
    return Some(labeling.getLabelAtRank(alphabet.size - 1))

    return Option.empty[Label]
  }


  def buildFromMotifs = {
    val motifs = Motif.build(this)
    channel = 9
    addNameEvent(t, "some track")
    t.add(TempoEvent(beatsPerMinute).toMidiEvent)
    printf("Realzing\n")
    motifs.realize((addNoteEvent _).curried(t))
  }

  def sampleMotif(numMeasures : Int, prev : Option[BaseMotif] = None)  : BaseMotif = {
    channel = 9
    var time = 0L
    val history =
      prev match {
        case Some(BaseMotif(_,_,_)) => prev.get.history.clone
        case None => new History(Data.historySize, alphabet, Generator.TICKS_PER_BEAT)
      }
    val r = new util.Random
    val maxTime = ticksPerBeat * 4 * numMeasures - ticksPerBeat
    val motif = Array.newBuilder[NoteEvent]

    while(time < maxTime) {
      sampleNextNote(r, history) match {
        case None =>
          // Try again
          return sampleMotif(numMeasures, prev)
          
        case Some(label) =>
          val (onoff, tick, key) = Data.decode(label)
          printf("\tNote: %s\n", label)
          if (tick >= 0) {
            printf("\t\tAdding: %d %d\n", tick, key)
            time += tick
            val event = onoff match {
              case "+" => NoteOn(time, key, 100)
              case "-" => NoteOff(time, key)
              case "/" => Skip(time)
            }
            motif += event
            history += event
          }
      }
    }

    return BaseMotif(motif.result, 0, history)
  }
}
