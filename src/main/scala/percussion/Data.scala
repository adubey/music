package ca.dubey.music.percussion

import ca.dubey.music.learn.Quantizer
import cc.mallet.types.Alphabet
import cc.mallet.types.Label
import cc.mallet.types.LabelAlphabet
import collection.mutable.Queue
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.Track

object Data {
  /*
  val velocityQuantizer = Quantizer(0, 16, 30, 48, 64, 80, 90, 100, 110, 123, 127)
  val tickQuantizer = Quantizer(0, 1, 2, 4, 12, 24, 32, 48, 96, 192)
  */
  val velocityQuantizer = Quantizer(0, 50, 100, 127)
  val tickQuantizer = Quantizer(0, 1, 12, 24, 48)

  val keyLow = 32
  val keyHigh = 82
  /** The range of keys is actually 33 to 81, but leave a low and high for unknown keys */
  val keyQuantizer = Quantizer(keyLow to keyHigh : _*)

  val noteLabel = raw"(\d+)_(\d+)_(\d+)".r

  // Time since last note on same key, normalized to PPQ=960
  val lastNoteQuantizer = Quantizer(
      480, 240, 960, 1920, 120, 200, 20, 320, 160, 1440, -960, 60, 720, 0, 40, 80, 
      3840, 32, 640, 30, 50, 192, 5, 100, 224, 472, 384, 2880, 128, 256, 110, 288, 96,
      1200, 352, 150, 448, 90, 800, 2400, 64, 180, 140, 35)

  val deltaQuantizer = Quantizer(320, 32, 80, 160, 200, 480, 120, 10, 240, 0, 960, 1140, 1840)

  val beatQuantizer = Quantizer(
      0, 480, 720, 240, 200, 640, 320, 120, 10, 800, 950, 160, 600, 500, 40, 840)

  val ids = Array("+", "-")

  def isKeyInRange(key : Int) = key > keyLow && key < keyHigh

  def normalizeTick(tick : Long, lastTick : Long, resolution : Int) : Int = {
    (0.5 + 96.0 * (tick - lastTick) / resolution.toFloat).toInt
  }

  def unnormalizeTick(delta : Int, resolution : Int) : Long = {
    (0.5 + (resolution.toDouble * delta) / 96.0).toLong
  }

  def encode(alphabet : LabelAlphabet, tick : Int, key : Int, velocity : Int, expand : Boolean = false) : Label = {
    alphabet.lookupLabel(
        "%d_%d_%d".format(
            tickQuantizer.quantize(tick),
            keyQuantizer.quantize(key),
            velocityQuantizer.quantize(velocity)),
          expand)
  }

  def encode(alphabet : LabelAlphabet, tick : String, key : String, velocity : String) : Label = {
    encode(alphabet, tick.toInt, key.toInt, velocity.toInt, false)
  }

  def decode(label : Label) : (Int, Int, Int) = {
    val s = label.getEntry.asInstanceOf[String]

    s match {
      case noteLabel(tick, key, velocity) => return (tick.toInt, key.toInt, velocity.toInt)
      case _ => return (0, 0, 0)
    }
  }

  def makeAlphabet : LabelAlphabet = {
    val alphabet = new LabelAlphabet
    alphabet.startGrowth
    alphabet.lookupLabel("START", true)
    alphabet.lookupLabel("STOP", true)
    for (tick <- tickQuantizer.set) {
      for (key <- keyQuantizer.set) {
        for (velocity <- velocityQuantizer.set) {
          encode(alphabet, tick, key, velocity, true)
        }
      }
    }
    alphabet.stopGrowth

    return alphabet
  }
}
