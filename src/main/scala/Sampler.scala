package ca.dubey.music

import javax.sound.midi.MetaEventListener
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiSystem

object Sampler {

  def main(args : Array[String]) : Unit = {

    val synth = MidiSystem.getSynthesizer
    synth.open
    val mc = synth.getChannels
    val instr = synth.getDefaultSoundbank.getInstruments
    synth.loadInstrument(instr(0))

    for (filename <- args) {
      printf("Loading table.\n")
      val chordTable = ChordTable.fromFile(filename)
      printf("Done.\n")
      var chordOption : Option[Chord] = Some(Chord("C"))
      for (songs <- 0 to 20) {
        for (notes <- 0 to 500) {
          for (chord <- chordOption) {
            chord.play(mc(5))
            printf("%s\n", chord)
            Thread.sleep(50)
            chordOption = chordTable.sample(chord)
          }
        }
      }
      Thread.sleep(1000)
    }
    System.exit(0)
  }
}
