package ca.dubey.music.midi

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.InvalidMidiDataException

object File {
  /**
   * Loads a sequence from the file system.
   * 
   * @returns None if an error occurs.
   */
  def loadSequence(filename : String) : Option[Sequence] = {
    try {
      val is = new BufferedInputStream(new FileInputStream(filename))
      val sequence = MidiSystem.getSequence(is)
      is.close
      Some(sequence)
      // Print events out from the sequence.
    } catch {
      case ex:InvalidMidiDataException =>
	printf("Couldn't load: %s\n", filename)
	ex.printStackTrace();
        None
      case ex:IOException =>
	printf("Couldn't load: %s\n", filename)
	ex.printStackTrace();
        None
    }
  }
}
