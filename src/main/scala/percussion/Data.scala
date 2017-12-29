package ca.dubey.music.percussion

import ca.dubey.music.learn.Quantize
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
  val velocityQuanta = Quantize(0, 16, 30, 48, 64, 80, 90, 100, 110, 123, 127)
  val tickQuanta = Quantize(0, 1, 2, 4, 12, 24, 32, 48, 96, 192)
  */
  val velocityQuanta = Quantize(0, 50, 100, 127)
  val tickQuanta = Quantize(0, 1, 12, 24, 48)

  val noteLow = 32
  val noteHigh = 82
  /** The range of notes is actually 33 to 81, but leave a low and high for unknown notes */
  val noteQuanta = Quantize(noteLow to noteHigh : _*)

  val noteLabel = raw"(\d+)_(\d+)_(\d+)".r

  def normalizeTick(tick : Long, lastTick : Long, resolution : Int) : Int = {
    (0.5 + 96.0 * (tick - lastTick) / resolution.toFloat).toInt
  }

  def unnormalizeTick(delta : Int, resolution : Int) : Long = {
    (0.5 + (resolution.toDouble * delta) / 96.0).toLong
  }

  def encode(alphabet : LabelAlphabet, tick : Int, note : Int, velocity : Int, expand : Boolean = false) : Label = {
    alphabet.lookupLabel(
        "%d_%d_%d".format(
            tickQuanta.quantize(tick),
            noteQuanta.quantize(note),
            velocityQuanta.quantize(velocity)),
          expand)
  }

  def encode(alphabet : LabelAlphabet, tick : String, note : String, velocity : String) : Label = {
    encode(alphabet, tick.toInt, note.toInt, velocity.toInt, false)
  }

  def decode(label : Label) : (Int, Int, Int) = {
    val s = label.getEntry.asInstanceOf[String]

    s match {
      case noteLabel(tick, note, velocity) => return (tick.toInt, note.toInt, velocity.toInt)
      case _ => return (0, 0, 0)
    }
  }

  def makeAlphabet : LabelAlphabet = {
    val alphabet = new LabelAlphabet
    alphabet.startGrowth
    alphabet.lookupLabel("START", true)
    alphabet.lookupLabel("STOP", true)
    for (tick <- tickQuanta.set) {
      for (note <- noteQuanta.set) {
        for (velocity <- velocityQuanta.set) {
          encode(alphabet, tick, note, velocity, true)
        }
      }
    }
    alphabet.stopGrowth

    return alphabet
  }
}
