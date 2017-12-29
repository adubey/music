package ca.dubey.music.midi

import ca.dubey.music.midi.event.TempoEvent
import javax.sound.midi.Sequence
import javax.sound.midi.Sequencer
import javax.sound.midi.Track
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

trait TrackBuilder {

  protected var channel : Int = 0

  protected def addNameEvent(t : Track, name : String) = {
    val trackName = new String(name)
    val mt = new MetaMessage()
    mt.setMessage(0x03 ,trackName.getBytes(), trackName.length())
    t.add(new MidiEvent(mt, 0L))
  }

  protected def addPolyOn(t : Track) : Unit = {
    t.add(new MidiEvent(new ShortMessage(ShortMessage.CONTROL_CHANGE, 0x75, 0x00), 0))
  }

  protected def addProgramChange(t : Track, program : Int) : Unit = {
    t.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, channel, program, 0), 0))
  }

  protected def addNote(t : Track, key : Int, dur : Int, time : Long, velocity : Int = 0x60) : Unit = {
    if (velocity == 0) {
      return
    }

    addNoteOn(t, key, time, velocity)
    addNoteOff(t, key, time + dur)
  }

  protected def makeNoteOn(key : Int, time : Long, velocity : Int) : MidiEvent = {
    return new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channel, key, velocity), time)
  }

  protected def addNoteOn(t : Track, key : Int, time : Long, velocity : Int = 0x60) : Unit = {
    if (velocity == 0) {
      addNoteOff(t, key, time)
      return
    }

    t.add(makeNoteOn(key, time, velocity))
  }

  protected def addNoteOff(t : Track, key : Int, time : Long) : Unit = {
    t.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channel, key, 0), time))
  }
}
