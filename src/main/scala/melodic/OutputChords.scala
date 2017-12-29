package ca.dubey.music.melodic

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MidiSystem

/** V1 */
/** Output the chords from a given MIDI input. */
object OutputChords extends App {
  for (filename <- args) {
    printf("Trying: %s\n", filename)
    try {
      val is = new BufferedInputStream(new FileInputStream(filename))
      val consumer = new ConsumeSequence(MidiSystem.getSequence(is))
      is.close
      consumer.consumeTracksIndependently
      // Print events out from the sequence.
      printf("Outputting: %s\n", filename)
      consumer.output
    } catch {
      case ex:InvalidMidiDataException =>
	printf("Couldn't load: %s\n", filename)
	ex.printStackTrace();
      case ex:IOException =>
	printf("Couldn't load: %s\n", filename)
	ex.printStackTrace();
    }
  }

  System.exit(0)
}
