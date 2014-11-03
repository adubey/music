package ca.dubey.music

import javax.sound.midi.MidiChannel

object Note {
  val notes = Array(
    "C",
    "C#",
    "D",
    "D#",
    "E",
    "F",
    "F#",
    "G",
    "G#",
    "A",
    "A#",
    "B"
  )

  def fromString : String => Note = {
    case "C"  => new Note(60)
    case "C#" => new Note(61)
    case "D"  => new Note(62)
    case "D#" => new Note(63)
    case "E"  => new Note(64)
    case "F"  => new Note(65)
    case "F#" => new Note(66)
    case "G"  => new Note(67)
    case "G#" => new Note(68)
    case "A"  => new Note(69)
    case "A#" => new Note(70)
    case "B"  => new Note(71)
    case _    => new Note(0)
  }

  def midiToString(midiNote : Int) : String = notes(midiNote % 12)
  def midiToOctave(midiNote : Int) : Int = midiNote / 12
}

case class Note(val value : Int) {
  val name = Note.midiToString(value)
  val octave = Note.midiToOctave(value)

  override def toString = name

  override def hashCode : Int = value

  override def equals(other : Any) : Boolean = {
    other match {
      case that:Note =>
	(that canEqual this) && that.value == this.value
      case _ => false
    }
  }

  def canEqual(other : Any) : Boolean = other.isInstanceOf[Note]

  def noteOn(mc : MidiChannel, velocity : Int = 60) = {
    mc.noteOn(value, velocity)
  }

  def noteOff(mc : MidiChannel, velocity : Int = 0) = {
    mc.noteOff(value, velocity)
  }
}
