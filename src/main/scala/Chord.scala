package ca.dubey.music

import javax.sound.midi.MidiMessage
import javax.sound.midi.ShortMessage
import javax.sound.midi.MidiChannel
import scala.collection.mutable.ArrayBuffer

object Chord {
  def apply(chord : Chord) : Chord = {
    new Chord(chord.notes.clone)
  }

  def apply() : Chord = {
    new Chord(ArrayBuffer.empty[Note])
  }

  def apply(s : String) : Chord = {
    val ss = collection.mutable.Set.empty[String]
    for (s <- s.split(" ")) {
      ss += s
    }
    val notes = ArrayBuffer.empty[Note]
    for (note <- ss) {
      notes += Note.fromString(note)
    }
    new Chord(notes)
  }
}

class Chord(val notes : ArrayBuffer[Note]) {

  override def hashCode : Int = notes.hashCode

  override def equals(other : Any) : Boolean = {
    other match {
      case that:Chord =>
        (that canEqual this) && that.notes == this.notes
      case _ => false
    }
  }

  def canEqual(other : Any) : Boolean = other.isInstanceOf[Chord]

  def output = {
    if (notes.size == 0) {
      printf("Play rest\n");
    } else {
      printf("Play %s\n", this.toString)
    }
  }

  def play(mc : MidiChannel) = {
    for (note <- notes) {
      note.noteOn(mc)
    }
    Thread.sleep(100)
    for (note <- notes) {
      note.noteOff(mc)
    }
  }

  def updateChordWithShortMessage(e : ShortMessage, state : ChannelState) = {
    e.getCommand match {
      case ShortMessage.NOTE_ON =>
        if (e.getData2 == 0) {
          notes -= Note(e.getData1)
        } else if (state.isMelodic) {
          // Only process melodic NOTE_ONs
          notes += Note(e.getData1)
        }
      case ShortMessage.NOTE_OFF =>
        notes -= Note(e.getData1)
      case _ =>
        ()
    }
  }

  override def toString = notes.map((note) => note.toString).mkString(" ")
}
