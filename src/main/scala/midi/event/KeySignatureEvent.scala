package ca.dubey.music.midi.event


import ca.dubey.music.theory.KeySignature
import ca.dubey.music.theory.Key
import ca.dubey.music.theory.Tonality
import ca.dubey.music.theory.Major
import ca.dubey.music.theory.Minor
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

object KeySignatureEvent extends EventCompanion[KeySignatureEvent] {
  val EVENT_ID = 0x59

  def fromMidiEventData(data : Array[Byte]) : Option[KeySignatureEvent] = {
    val tonality = Tonality.fromByte(data(1))
    val k = KeySignature.fromTonalityAndNumAccidentals(tonality, data(0))
    return Some(new KeySignatureEvent(k))
  }

  def apply(n : Key, t : Tonality) : KeySignatureEvent = {
    new KeySignatureEvent(new KeySignature(n, t))
  }
}

class KeySignatureEvent(val keySignature : KeySignature) extends Event {
  override def toMidiEvent : MidiEvent = {
    return new MidiEvent(
        new MetaMessage(
            KeySignatureEvent.EVENT_ID,
            Array[Byte](
              keySignature.numAccidentals.toByte,
              keySignature.tonality.toByte),
            2),
        0L)
  }
}
