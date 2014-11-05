package ca.dubey.music

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MidiSystem

object OutputChords {
  def main(args : Array[String]) : Unit = {
    for (filename <- args) {
      try {
        val is = new BufferedInputStream(new FileInputStream(filename))
        val consumer = new ConsumeSequence(MidiSystem.getSequence(is))
        is.close
        // Print events out from the sequence.
        printf("Outputting: %s\n", output)
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
  }

  System.exit(0)
}
