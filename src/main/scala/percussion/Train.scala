package ca.dubey.music.percussion

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

object Train extends App {
  val eventsFile = args(0)
  val outputFile = args(1)
  val maxLength = 4

  def loadInstances(filename : String) : Train = {
    val data = new Train(new LabelAlphabet, new LabelAlphabet)
    data.loadInstances(filename)
    data
  }

  val data = loadInstances(eventsFile)

  val trainer = new MaxEntTrainer()
  val classifier = trainer.train(data.instanceList)

  val ois = new ObjectOutputStream (new FileOutputStream(outputFile));
  ois.writeObject(classifier)
  ois.close();

  class History(val size : Int, val alphabet : Alphabet) {
    val contents = Array.fill(size)("START")

    def +=(data : String) = {
      for (i <- 1 until size) {
        contents(i) = contents(i-1)
      }
      contents(0) = data
    }

    def featureVector : FeatureVector = {
      val d = Array.fill(size)(0)
      for (i <- 0 until size) {
        d(i) = alphabet.lookupLabel("%d_%s".format(i, contents(i))).getIndex
      }
      return new FeatureVector(alphabet, d)
    }
  }
}

class Train(val alphabet : LabelAlphabet, val targetAlphabet : LabelAlphabet, val maxLength : Int) {
  val noteOnEvent = raw"\+ (\d+) (\d+) (\d+)".r
  val noteOffEvent = raw"- (\d+) (\d+)".r
  val instanceList = new InstanceList(alphabet, alphabet)
  val h = new History(maxLength, alphabet)

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
      case noteOnEvent(tick, note, velocity) => Data.encode(alphabet, tick, note, velocity)
      case noteOffEvent(tick, note) => Data.encode(alphabet, tick, note, "0")
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
          // Reset.
          h = new History(maxLength, alphabet)
          anyNotes = false
        }
      } else if (!line.startsWith("Info")) {
        nextTrainingExample(convertToFeature(line))
        anyNotes = true
      }
    }
    for (lineRaw <- Source.fromFile(filename).getLines) {
      numLines += 1
      val line = lineRaw.trim
      if (line.startsWith("Song")) {
        numSongs += 1
        if (!skipSong) {
          if (notes.isEmpty) {
            numEmpty += 1
          } else {
            currentSong.analyze
          }
        }
        skipSong = false
        notes = ArrayBuffer.empty[NoteEvent]
        currentSong = Analyzer.Song(notes)
      } else {
        if (!skipSong) {
          line match {
            case noteOnEvent(tick, note, velocity) =>
              val n = note.toInt
              if (Data.isKeyInRange(n)) {
                notes += NoteOn(tick.toInt, n, velocity.toInt)
              } else {
                notes += Skip(tick.toInt)
              }
            case noteOffEvent(tick, note) =>
              val n = note.toInt
              if (Data.isKeyInRange(n)) {
                notes += NoteOff(tick.toInt, n)
              } else {
                notes += Skip(tick.toInt)
              }
            case timing(x, y) =>
              if (x.toFloat != 0.0) {
                skipSong = true
                numSkipped += 1
              } else {
                currentSong.setPpq(y.toInt)
              }
            case timeSignature(n, d) =>
              if (d != "4" && d != "8") {
                skipSong = true
                numSkipped += 1
              } else {
                currentSong.setTimeSignature(n.toInt, d.toInt)
              }
            case _ =>  ()
          }
        }
      }
    }
  }
}

