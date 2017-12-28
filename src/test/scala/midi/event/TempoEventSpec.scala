import org.scalatest.FlatSpec
import ca.dubey.music.midi.event.TempoEvent
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

class TempoEventSpec extends FlatSpec {
  "A TempoEvent" should "convert from MidiEvent" in {
    val midiEvent =
      new MidiEvent(
        new MetaMessage(
          TempoEvent.EVENT_ID,
          Array[Byte](0x07, 0xA1.toByte, 0x20),
          3),
        0)

    assert(TempoEvent.fromMidiEvent(midiEvent).get.bpm == 120)
  }

  "A TempoEvent" should "not lose data when converted to and from a MidiEvent" in {
    val beatsPerMinute = 90
    val tempoEvent = new TempoEvent(beatsPerMinute)
    assert(TempoEvent.fromMidiEvent(tempoEvent.toMidiEvent).get.bpm == beatsPerMinute)
  }
}

