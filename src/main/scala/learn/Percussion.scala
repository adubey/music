package ca.dubey.music.learn

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.SequencePlayer
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import cc.mallet.classify.MaxEnt
import cc.mallet.types.Alphabet
import cc.mallet.types.Label
import cc.mallet.types.LabelAlphabet
import cc.mallet.types.FeatureVector
import cc.mallet.types.Instance
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

object Percussion extends App {
  val modelFilename = args(0)
  val TICKS_PER_BEAT = 24
  val sequencer = MidiSystem.getSequencer
  sequencer.open

  val ois = new ObjectInputStream (new FileInputStream(modelFilename));
  val classifier = ois.readObject.asInstanceOf[MaxEnt]
  ois.close

  val builder = new Percussion(TICKS_PER_BEAT, 90, classifier)
  builder.build
  builder.play(sequencer)

  System.exit(0)
}

class Percussion(
    val ticksPerBeat : Int,
    val tempo : Int,
    val classifier : MaxEnt)
    extends SequencePlayer with ca.dubey.music.midi.TrackBuilder {

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
      printf("So far: %f; largest: %f; rank: %d; ev: %s %s\n", targetProb, p, j, s1, s2)
      if (p >= targetProb) {
        printf("\tFound\n")
        return Some(label)
      }
    }
    return Some(labeling.getLabelAtRank(alphabet.size - 1))

    printf("Didn't find: %f; largest: %f\n", targetProb, p)
    return Option.empty[Label]
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
          val (tick, note, velocity) = PercussionData.decode(label)
          if (tick == 0 && note == 0 && velocity == 0) {
            printf("Stopped early: %s\n", label)
            return t
          }
          time += PercussionData.unnormalizeTick(tick, ticksPerBeat)
          addNoteOn(t, note, time, velocity)
          q += label
          q.dequeue
      }

    }

    return t
  }
}
