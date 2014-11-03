package ca.dubey.music

import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import javax.sound.midi.MidiSystem
import javax.sound.midi.MidiUnavailableException
import javax.sound.midi.Sequence
import javax.sound.midi.Sequencer
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MetaEventListener
import javax.sound.midi.MetaMessage

object MidiPlayer {
  // Midi meta event
  val END_OF_TRACK_MESSAGE : Int = 47
}

class MidiPlayer extends MetaEventListener {

  var sequencer : Sequencer = null
  private var loop : Boolean = false
  private var paused : Boolean = false

  /**
   * Creates a new MidiPlayer object.
   */
  def init = {
    try {
      sequencer = MidiSystem.getSequencer
      sequencer.open();
      sequencer.addMetaEventListener(this);
    } catch {
      case ex:MidiUnavailableException =>
	sequencer = null;
    }
  }

  /**
   * Loads a sequence from the file system. Returns null if an error occurs.
   */
  def loadSequence(filename : String) : Option[Sequence] = {
    try {
      return loadSequence(new FileInputStream(filename));
    } catch {
      case ex:IOException =>
	ex.printStackTrace
	return None
    }
  }

  /**
   * Loads a sequence from an input stream. Returns null if an error occurs.
   */
  def loadSequence(is : InputStream) : Option[Sequence] = {
    try {
      val buffered_is =
	if (!is.markSupported) {
	  new BufferedInputStream(is)
	} else {
	  is
	}
      val s = MidiSystem.getSequence(is);
      buffered_is.close
      return Some(s);
    } catch {
      case ex:InvalidMidiDataException =>
	ex.printStackTrace();
	return None
      case ex:IOException =>
	ex.printStackTrace();
	return None
    }
  }

  /**
   * Plays a sequence, optionally looping. This method returns immediately.
   * The sequence is not played if it is invalid.
   */
  def play(sequence : Sequence, loop : Boolean) : Unit = {
    if (sequencer == null || sequence == null || !sequencer.isOpen) {
      return
    }

    val tracks = sequence.getTracks
    printf("There are %d tracks\n", tracks.size)
    for (i <- 0 until tracks.size) {
      printf("Track %d has %d events and %d ticks\n", i, tracks(i).size, tracks(i).ticks)
    }
    var tick = 0
    val trackCounter = tracks.map((x) => x.get(0).getTick)

    /*
    try {
      sequencer.setSequence(sequence)
      sequencer.start
      this.loop = loop
    } catch {
      case ex:InvalidMidiDataException =>
	ex.printStackTrace
    }
    */
  }

  /**
   * This method is called by the sound system when a meta event occurs. In
   * this case, when the end-of-track meta event is received, the sequence is
   * restarted if looping is on.
   */
  def meta(event : MetaMessage) : Unit  = {
    if (event.getType == 47) {
      if (sequencer != null && sequencer.isOpen() && loop) {
        sequencer.start
      }
    }
  }

  /**
   * Stops the sequencer and resets its position to 0.
   */
  def stop : Unit = {
    if (sequencer != null && sequencer.isOpen()) {
      sequencer.stop();
      sequencer.setMicrosecondPosition(0);
    }
  }

  /**
   * Closes the sequencer.
   */
  def close : Unit = {
    if (sequencer != null && sequencer.isOpen()) {
      sequencer.close();
    }
  }

  /**
   * Gets the sequencer.
   */
  def getSequencer : Sequencer = {
    return sequencer;
  }

  /**
   * Sets the paused state. Music may not immediately pause.
   */
  def setPaused(paused : Boolean) : Unit = {
    if (this.paused != paused && sequencer != null && sequencer.isOpen()) {
      this.paused = paused;
      if (paused) {
        sequencer.stop();
      } else {
        sequencer.start();
      }
    }
  }
}
