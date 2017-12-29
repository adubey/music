package ca.dubey.music.theory

import javax.sound.midi.MidiChannel

object Key {
  val keysWithSharps = Array(
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

  val keysWithFlats = Array(
    "C",
    "Db",
    "D",
    "Eb",
    "E",
    "F",
    "Fb",
    "G",
    "Gb",
    "A",
    "Bb",
    "B"
  )

  val keys = keysWithSharps

  def fromString : String => Key = {
    case "C"  => new Key(60)
    case "C#" => new Key(61)
    case "Db" => new Key(61)
    case "D"  => new Key(62)
    case "D#" => new Key(63)
    case "Eb" => new Key(63)
    case "E"  => new Key(64)
    case "F"  => new Key(65)
    case "F#" => new Key(66)
    case "Gb" => new Key(66)
    case "G"  => new Key(67)
    case "G#" => new Key(68)
    case "Ab" => new Key(68)
    case "A"  => new Key(69)
    case "A#" => new Key(70)
    case "Bb" => new Key(70)
    case "B"  => new Key(71)
    case _    => new Key(0)
  }

  def midiToString(midiKey : Int) : String = keys(midiKey % 12)
  def midiToOctave(midiKey : Int) : Int = midiKey / 12
}

/**
  * Represent a key (a note's pitch).
  */
case class Key(val value : Int) {
  val name = Key.midiToString(value)
  val octave = Key.midiToOctave(value)

  override def toString = name

  override def hashCode : Int = value

  override def equals(other : Any) : Boolean = {
    other match {
      case that:Key =>
	(that canEqual this) && that.value == this.value
      case _ => false
    }
  }

  def canEqual(other : Any) : Boolean = other.isInstanceOf[Key]
}
