package ca.dubey.music.melodic

import ca.dubey.music.prob.ChordTable
import javax.sound.midi.Sequencer
import javax.sound.midi.MidiSystem

object Generate extends App {

    for (arg <- args) {
      val table = ChordTable.fromFile(arg)
      val TICKS_PER_BEAT = 24
      val sequencer = MidiSystem.getSequencer
      sequencer.open

      val builder = new TrackBuilder(TICKS_PER_BEAT, 60, table)
      builder.build
      builder.play(sequencer)

      System.exit(0)
    }
}
