package ca.dubey.music.melodic

import ca.dubey.music.prob.ChordTable
import javax.sound.midi.Sequencer
import javax.sound.midi.MidiSystem

object Generate extends App {

    for (arg <- args) {
      val table = ChordTable.fromFile(arg)
      val TICKS_PER_BEAT = 24
      val BEATS_PER_MINUTE = 60
      val sequencer = MidiSystem.getSequencer
      sequencer.open

      val builder = new TrackGenerator(TICKS_PER_BEAT, BEATS_PER_MINUTE, table)
      builder.build
      builder.play(sequencer)

      System.exit(0)
    }
}
