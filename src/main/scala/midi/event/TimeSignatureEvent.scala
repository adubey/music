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
    val bt : Array[Byte] = Array[Byte](0x04, n, d, metronome, eight)
    val mt = new MetaMessage()
    mt.setMessage(TimeSignatureEvent.EVENT_ID, bt, 4)
    return new MidiEvent(mt, 0L)
  }

  override def toString : String = {
    "Time Signature %d / %d".format(n, d)
  }
}
