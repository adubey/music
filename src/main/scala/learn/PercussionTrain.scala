package ca.dubey.music.learn

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.File
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import cc.mallet.classify.ClassifierTrainer
import cc.mallet.classify.MaxEntTrainer
import cc.mallet.classify.MaxEnt
import cc.mallet.types.Alphabet
import cc.mallet.types.FeatureVector
import cc.mallet.types.Instance
import cc.mallet.types.InstanceList
import cc.mallet.types.Label
import cc.mallet.types.LabelAlphabet
import collection.mutable.Queue
import io.Source
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.Track

object PercussionTrain extends App {
  val eventsFile = args(0)
  val outputFile = args(1)
  val maxLength = 4

  def loadInstances(filename : String) : PercussionTrain = {
    val alphabet = PercussionData.makeAlphabet
    val data = new PercussionTrain(PercussionData.makeAlphabet, maxLength)
    data.loadInstances(filename)
    data
  }

  val data = loadInstances(eventsFile)

  val trainer = new MaxEntTrainer()
  val classifier = trainer.train(data.instanceList)

  val ois = new ObjectOutputStream (new FileOutputStream(outputFile));
  ois.writeObject(classifier)
  ois.close();
}

class PercussionTrain(val alphabet : LabelAlphabet, val maxLength : Int) {
  val noteOnEvent = raw"\+ (\d+) (\d+) (\d+)".r
  val noteOffEvent = raw"- (\d+) (\d+)".r
  val instanceList = new InstanceList(alphabet, alphabet)
  val q = new Queue[Label]

  def fillQueue(label : String) : Unit = {
    while (q.size < maxLength) {
      q += alphabet.lookupLabel(label)
    }
  }

  def nextTrainingExample(s : String) : Unit = {
    nextTrainingExample(alphabet.lookupLabel(s))
  }

  def nextTrainingExample(next : Label) : Unit = {
    val vector = new FeatureVector(alphabet, q.map((x:Label) => x.getIndex).toArray)
    val instance = new Instance(vector, next, "", "")
    instanceList.add(instance)
    q += next
    q.dequeue
  }

  def convertToFeature(line : String) : Label = {
    line match {
      case noteOnEvent(tick, note, velocity) => PercussionData.encode(alphabet, tick, note, velocity)
      case noteOffEvent(tick, note) => PercussionData.encode(alphabet, tick, note, "0")
    }
  }

  def loadInstances(filename : String) : Unit = {
    fillQueue("START")
    var numLines = 0
    var anyNotes = false

    for (lineRaw <- Source.fromFile(filename).getLines) {
      numLines += 1
      if (numLines % 50000 == 0) {
        printf("At %d\n", numLines)
      }
      if (numLines % 500000 == 0) {
        return
      }
      val line = lineRaw.trim
      if (line.startsWith("Song")) {
        // Add a STOP
        if (anyNotes) {
          nextTrainingExample("STOP")
          // Fill with START
          for (i <- 1 to maxLength) {
            q += alphabet.lookupLabel("START")
            q.dequeue
          }
          anyNotes = false
        }
      } else if (!line.startsWith("Info")) {
        nextTrainingExample(convertToFeature(line))
        anyNotes = true
      }
    }
  }

}

