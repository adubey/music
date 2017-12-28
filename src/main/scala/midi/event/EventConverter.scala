package ca.dubey.music.midi

import javax.sound.midi.MidiEvent

trait EventConverter {
  def toMidiEvent : MidiEvent
}
