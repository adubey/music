package ca.dubey.music.percussion

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.SequencePlayer
import ca.dubey.music.midi.TrackBuilder
import ca.dubey.music.midi.event.EventConsumer
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

object Generator extends App {
  val modelFilename = args(0)
  val TICKS_PER_BEAT = 24
  val sequencer = MidiSystem.getSequencer
  sequencer.open

  val ois = new ObjectInputStream (new FileInputStream(modelFilename));
  val classifier = ois.readObject.asInstanceOf[MaxEnt]
  ois.close

  val builder = new Generator(TICKS_PER_BEAT, 120, classifier)
  builder.buildFromMotifs
  builder.play(sequencer)

  System.exit(0)
}

class Generator(
    val ticksPerBeat : Int,
    val beatsPerMinute : Int,
    val classifier : MaxEnt)
    extends SequencePlayer with TrackBuilder {

  var maxLength = 4
  protected val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  protected val t = sequence.createTrack
  private val alphabet : LabelAlphabet = classifier.getAlphabet.asInstanceOf[LabelAlphabet]

  def sampleNextNote(r : util.Random, q : Queue[Label]) : Option[Label] = {
    val vector = new FeatureVector(alphabet, q.map((x:Label) => x.getIndex).toArray)
    val instance = new Instance(vector, null, "", "")
    val classification = classifier.classify(instance)
    val labeling = classification.getLabeling
    var p = 0D
    val targetProb = r.nextDouble
    for (j <- 0 until alphabet.size) {
      p += labeling.getValueAtRank(j)
      val label = labeling.getLabelAtRank(j)
      // val string = alphabet.lookupObject(label.getIndex).asInstanceOf[String]
      val s1 = label.getEntry.asInstanceOf[String]
      val s2 = alphabet.lookupObject(label.getIndex).asInstanceOf[String]
      if (p >= targetProb) {
        return Some(label)
      }
    }
    return Some(labeling.getLabelAtRank(alphabet.size - 1))

    return Option.empty[Label]
  }

  case class NoteEvent(val offset : Long, val note : Int, val velocity : Int) {
    override def toString : String =
      if (velocity > 0) { "N(%d,%d)".format(offset,note) } else ""
  }

  abstract class Motif {
    var offset : Long
    def end : Long
    def realize(additionalOffset : Long = 0) : Unit
  }

  case class BaseMotif(val events : Array[NoteEvent], var offset : Long = 0) extends Motif {
    override def end = events(events.size-1).offset + offset

    override def clone : BaseMotif = BaseMotif(events, offset)

    override def realize(additionalOffset : Long = 0) : Unit = {
      for (note <- events) {
        addNoteOn(t, note.note, note.offset + offset + additionalOffset, note.velocity)
        printf("Realizing %d+%s\n", offset+additionalOffset, note)
      }
    }
  }

  case class ComposedMotif(
      var motifs : ListBuffer[BaseMotif],
      var offset : Long = 0) extends Motif {
    override def end = if(motifs.isEmpty) { offset } else { motifs(motifs.size-1).end }

    override def clone : ComposedMotif = {
      ComposedMotif((motifs.map((m:BaseMotif) => m.clone)), offset)
    }

    override def realize(additionalOffset : Long = 0) : Unit = {
      for (motif <- motifs) {
        // This's offset should already be set via +=
        motif.realize(additionalOffset)
      }
    }

    def last : Option[BaseMotif] = motifs.isEmpty match {
      case true => None
      case false => Some(motifs(motifs.size-1))
    }

    def +=(add : Motif) : ComposedMotif = {
      add match {
        case b:BaseMotif =>
          add.offset += end
          motifs += b
        case c:ComposedMotif =>
          val cloned = c.motifs.map((m:BaseMotif) => m.clone)
          for (motif <- cloned){
            motif.offset += end
          }
          motifs ++= cloned
      }
      return this
    }
  }

  def buildFromMotifs = {
    // Warm it up.
    val start = buildMotif(20)
    // This is the actual begining.
    val a1 = buildMotif(1, Some(start))
    val bases = Array(extendMotif(a1, 1), extendMotif(a1, 1), extendMotif(a1, 1), extendMotif(a1, 1))
    var r = new util.Random

    val motifs = ComposedMotif(ListBuffer.empty[BaseMotif])

    for (i <- 1 to 20) {
      if (i % 2 == 0) {
        val j = r.nextInt(bases.size)

        motifs += bases(j).clone
      } else {
        motifs += buildMotif(2, motifs.last)
      }
    }

    channel = 9
    addNameEvent(t, "some track")
    t.add(TempoEvent(beatsPerMinute).toMidiEvent)
    motifs.realize(0)
  }

  def extendMotif(motif : Motif, numMeasures : Int) : ComposedMotif = {
    motif match {
      case b:BaseMotif =>
        val next = buildMotif(numMeasures, Some(b))
        val composed = ComposedMotif(ListBuffer.empty[BaseMotif])
        composed += b.clone
        composed += next
        return composed
      case c:ComposedMotif =>
        c += buildMotif(numMeasures, c.last)
        return c
    }
  }
 
  def buildMotif(numMeasures : Int, prev : Option[BaseMotif] = None)  : BaseMotif = {
    channel = 9
    var time = 0L
    val q = new Queue[Label]
    val r = new util.Random
    val maxTime = ticksPerBeat * 4 * numMeasures - ticksPerBeat
    val motif = Array.newBuilder[NoteEvent]

    while (q.size < maxLength) {
      q += alphabet.lookupLabel("START")
    }

    for (p <- prev) {
      val windowStart = p.events.size - q.size
      var offset = 0L
      // This has a timing bug if p.size > q.size
      for (i <- math.max(0, windowStart - 1) until q.size) {
        val event = p.events(i)
        q += alphabet.lookupLabel(Data.encode(alphabet, (event.offset - offset).toInt, event.note, event.velocity))
        q.dequeue
        offset = event.offset
      }
    }

    while(time < maxTime) {
      sampleNextNote(r, q) match {
        case None =>
          // Try again
          return buildMotif(numMeasures)
          
        case Some(label) =>
          val (tick, note, velocity) = Data.decode(label)
          if (tick == 0 && note == 0 && velocity == 0) {
            printf("Stopped early: %s\n", label)
            // Try again
            return buildMotif(numMeasures)
          }
          time += Data.unnormalizeTick(tick, ticksPerBeat)
          motif += NoteEvent(time, note, velocity)
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

    while (q.size < maxLength) {
      q += alphabet.lookupLabel("START")
    }

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
