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
    val tonality = if (data(1) == 0) { Major } else { Minor }
    val baseKey = KeySignature.baseKeyFromTonalityAndNumAccidentals(tonality, data(0))
    return Some(new KeySignatureEvent(baseKey, tonality, data(0)))
  }
}

class KeySignatureEvent(n : Key, t : Tonality, a : Byte) extends Event {

  val keySignature = new KeySignature(n, t)

  override def toMidiEvent : MidiEvent = {
    val mt = new MetaMessage()
    mt.setMessage(
        KeySignatureEvent.EVENT_ID,
        Array[Byte](a, t match { case Major => 0 case Minor => 1 }),
        2)
    return new MidiEvent(mt, 0L)
  }
}
