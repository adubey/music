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
  val historySize = 3

  def loadInstances(filename : String) : Train = {
    val train = new Train(new LabelAlphabet, new LabelAlphabet, historySize)
    val analyzer = new Analyzer
    analyzer.analyzeSongs(filename, train.process, Some(200000))
    return train
  }

  val data = loadInstances(eventsFile)

  val trainer = new MaxEntTrainer()
  val classifier = trainer.train(data.instanceList)

  val ois = new ObjectOutputStream (new FileOutputStream(outputFile));
  ois.writeObject(classifier)
  ois.close();
}

class Train(val alphabet : LabelAlphabet, val targetAlphabet : LabelAlphabet, val historySize : Int) {
  val instanceList = new InstanceList(alphabet, targetAlphabet)

  def process(song : NoteIterable) : Unit = {
    for (instance <- song.extractInstances(alphabet, targetAlphabet, historySize)) {
      instanceList.add(instance)
    }
  }
}

