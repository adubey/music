package ca.dubey.music.midi.event

import ca.dubey.music.theory.Tempo
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

object TempoEvent extends EventCompanion[TempoEvent] {
  val EVENT_ID = 0x51

  val MS_IN_TICKS : Long = 60000000L

  def fromMidiEventData(data : Array[Byte]) : Option[TempoEvent] = {
    val midiTempo = ((data(0) & 0xFF) << 16) |
                    ((data(1) & 0xFF) << 8) |
                     (data(2) & 0xFF)
    Some(new TempoEvent((MS_IN_TICKS / midiTempo).toInt))
  }

}

case class TempoEvent(bpm : Int) extends Event {
  val tempo = new Tempo(bpm)
  private val HIGH_BITS = 0xFF0000
  private val MID_BITS = 0x00FF00
  private val LOW_BITS = 0x0000FF

  override def toMidiEvent : MidiEvent = {
    var midiTempo = TempoEvent.MS_IN_TICKS / bpm;
    // assert(0 < midiTempo && midiTempo < 0xFFFFFF)
    return new MidiEvent(
      new MetaMessage(
        TempoEvent.EVENT_ID,
        Array[Byte](
          ((midiTempo & HIGH_BITS) >> 16).asInstanceOf[Byte],
          ((midiTempo &  MID_BITS) >> 8).asInstanceOf[Byte],
          ((midiTempo & LOW_BITS)).asInstanceOf[Byte]),
        3),
      ticks)
  }

  override def toString : String = {
    "BPM %d".format(bpm)
  }
}
