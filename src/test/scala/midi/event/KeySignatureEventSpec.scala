import org.scalatest.FlatSpec
import ca.dubey.music.midi.event.KeySignatureEvent
import ca.dubey.music.theory.Key
import ca.dubey.music.theory.KeySignature
import ca.dubey.music.theory.Tonality
import ca.dubey.music.theory.Major
import ca.dubey.music.theory.Minor
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

class TimeSignatureEventSpec extends FlatSpec {
  "A TimeSignatureEvent" should "not lose data when converted to and from a MidiEvent" in {
    for (key <- Array("A", "B", "C", "D", "E", "F", "G")) {
      for (tonality <- Array(Major, Minor)) {
        val event = new KeySignatureEvent(
            new KeySignature(Key.fromString(key), tonality))
        val converted = KeySignatureEvent.fromMidiEvent(event.toMidiEvent).get

        assert(converted.keySignature.baseKey == event.keySignature.baseKey)
        assert(converted.keySignature.tonality == event.keySignature.tonality)
      }
    }
  }
}
