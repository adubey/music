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

object Train2 extends App {
  val eventsFile = args(0)
  val outputFile = args(1)
  val maxLength = 4

  def loadInstances(filename : String) : Train2 = {
    val dataAlphabet = new LabelAlphabet
    val targetAlphabet = new LabelAlphabet
    val data = new Train2(dataAlphabet, targetAlphabet)
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

class Train2(val dataAlphabet : LabelAlphabet, val targetAlphabet : LabelAlphabet) {
  val instanceList = new InstanceList(dataAlphabet, targetAlphabet)

  def loadInstances(filename : String) : Unit = {
    var numLines : Long = 0
    for (line <- Source.fromFile(filename).getLines) {
      numLines += 1
      if (numLines % 50000 == 0) {
        printf("At %d\n", numLines)
      }
      if (numLines % 200000 == 0) {
        return
      }
      val output = Analyzer.Output.fromString(line.trim)
      instanceList.add(output.encode(dataAlphabet, targetAlphabet))
    }
  }

}

