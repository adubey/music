package ca.dubey.music.midi.event

import javax.sound.midi.MidiEvent
import javax.sound.midi.MetaMessage

trait EventCompanion[E] {
  val EVENT_ID : Int

  def fromMidiEvent(m : MidiEvent) : Option[E] = {
    m.getMessage match {
      case e:MetaMessage if (e.getType == EVENT_ID) =>
        return fromMidiEventData(e.getData)
      case _ =>
        return None
    }
  }

  def fromMidiEventData(data : Array[Byte]) : Option[E]
  
}

abstract class Event {
  var ticks : Long = 0

  def toMidiEvent : MidiEvent
}
