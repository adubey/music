package ca.dubey.music

import javax.sound.midi.MetaEventListener
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiSystem

object Music extends MetaEventListener {

  def main(args : Array[String]) : Unit = {
    /*
    for(i <- 52 to 64) {
      val scale = new MajorKey(i)
      printf("%s\n", scale.toString)
    }
    */

    val synth = MidiSystem.getSynthesizer
    synth.open
    val mc = synth.getChannels
    val instr = synth.getDefaultSoundbank.getInstruments

    /*
    synth.loadInstrument(instr(0))
    mc(5).noteOn(60, 60)
    Thread.sleep(100)
    mc(5).noteOff(60, 60)
    */

    player.init
    
    for (filename <- args) {
      // load a sequence
      player.loadSequence(filename) match {
	case None =>
	  printf("Couldn't load %s\n", filename)
	case Some(sequence) =>
	  // Print events out from the sequence.
	  val consumer = new ConsumeSequence(sequence)
	  consumer.consume
	  // consumer.play(mc(5))
	  consumer.output
      }
    }
    System.exit(0)
  }

  def meta(event : MetaMessage) : Unit = {
    if (event.getType == MidiPlayer.END_OF_TRACK_MESSAGE) {
      val sequencer = player.sequencer
      if (sequencer.getTrackMute(DRUM_TRACK)) {
        // turn on the drum track
        println("Turning on drums...")
        sequencer.setTrackMute(DRUM_TRACK, false)
      } else {
        // close the sequencer and exit
        println("Exiting...")
        player.close
        System.exit(0)
      }
    }
  }


  // The drum track in the example Midi file
  private val DRUM_TRACK : Int = 1
  private var player = new MidiPlayer

}
