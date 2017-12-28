package ca.dubey.music.midi

import ca.dubey.music.midi.event.EndTrackEvent
import javax.sound.midi.MidiSystem
import javax.sound.midi.MidiUnavailableException
import javax.sound.midi.Sequence
import javax.sound.midi.Sequencer
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MetaEventListener
import javax.sound.midi.MetaMessage

trait SequencePlayer {
  protected val sequence : Sequence

  def play(sequencer : Sequencer) = {
    sequencer.setSequence(sequence)
    sequencer.start()

    Thread.sleep(sequence.getMicrosecondLength / 1000 + 500)

    sequencer.stop()
  }
}
