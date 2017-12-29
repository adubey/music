package ca.dubey.music.midi.event

import ca.dubey.music.theory.Tempo
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

object EndTrackEvent extends EventCompanion[EndTrackEvent] {
  val EVENT_ID = 0x2F

  def fromMidiEventData(data : Array[Byte]) : Option[EndTrackEvent] = None
}

case class EndTrackEvent(bpm : Int) extends Event {

  override def toMidiEvent : MidiEvent = {
    return new MidiEvent(
      new MetaMessage(EndTrackEvent.EVENT_ID, Array[Byte](), 0),
      ticks)
  }
}
