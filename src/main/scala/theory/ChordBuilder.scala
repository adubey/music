package ca.dubey.music.theory

import ca.dubey.music.midi.ChannelInfo
import javax.sound.midi.ShortMessage
import scala.collection.mutable.ArrayBuffer

case class ChordBuilder(val notes : ArrayBuffer[Key] = ArrayBuffer.empty[Key]) {
  def +=(e : ShortMessage, info : ChannelInfo) : Unit = {
    e.getCommand match {
      case ShortMessage.NOTE_ON =>
	if (!info.isMelodic) {
	  return;
	}
        if (e.getData2 == 0) {
	  // Treat a silent NOTE ON is a note off.
          notes -= Key(e.getData1)
        } else {
          // Only process melodic NOTE_ONs
	  val note = Key(e.getData1)
	  if (!notes.contains(note)) {
	    notes += note
	  }
        }
      case ShortMessage.NOTE_OFF =>
	if (!info.isMelodic) {
	  return;
	}
        notes -= Key(e.getData1)
      case _ =>
        ()
    }
  }

  def result = Chord(notes.toList)

  def isEmpty = notes.isEmpty

  override def toString = notes.map((note) => note.toString).mkString(" ")
}
