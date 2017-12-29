package ca.dubey.music.midi.event


import ca.dubey.music.theory.TimeSignature
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

object TimeSignatureEvent extends EventCompanion[TimeSignatureEvent] {
  val EVENT_ID = 0x58

  def fromMidiEventData(data : Array[Byte]) : Option[TimeSignatureEvent] = {
    return Some(new TimeSignatureEvent(data(0), data(1), data(2), data(3)))
  }
}

class TimeSignatureEvent(
    n : Byte,
    d : Byte,
    metronome : Byte,
    eight : Byte) extends Event {

  val timeSignature = new TimeSignature(n, 2 << (d-1))

  override def toMidiEvent : MidiEvent = {
    return new MidiEvent(
        new MetaMessage(
            TimeSignatureEvent.EVENT_ID,
            Array[Byte](n, d, metronome, eight),
            4),
        0L)
  }

  override def toString : String = {
    "Time Signature %d / %d".format(n, d)
  }
}
