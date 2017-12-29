package ca.dubey.music.theory

import ca.dubey.music.midi.ChannelInfo
import javax.sound.midi.ShortMessage
import scala.collection.mutable.ArrayBuffer

object Chord {
  def apply() : Chord = new Chord(List.empty[Key])

  def apply(notes : Array[Key]) : Chord = new Chord(notes.toList)
  def apply(notes : List[Key]) : Chord = new Chord(notes)

  /**
    * Construct a Chord from a string of notes
    *
    * Example usage: Chord("C E G")
    */
  def apply(s : String) : Chord = {
    val ss = collection.mutable.Set.empty[String]
    for (s <- s.split(" ")) {
      ss += s
    }
    val notes = List.newBuilder[Key]
    for (note <- ss) {
      notes += Key.fromString(note)
    }
    new Chord(notes.result)
  }
}

class Chord(val notes : List[Key]) {

  override def hashCode : Int = notes.hashCode

  override def equals(other : Any) : Boolean = {
    other match {
      case that:Chord =>
        (that canEqual this) && that.notes.equals(this.notes)
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

  def simplify = Chord(notes.take(2))

  override def toString = notes.map((note) => note.toString).mkString(" ")
}
