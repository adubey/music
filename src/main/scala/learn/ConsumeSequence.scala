package ca.dubey.music.learn

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.Patch
import ca.dubey.music.theory.Chord
import ca.dubey.music.theory.ChordBuilder
import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiChannel
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.Track
import scala.collection.mutable.ArrayBuffer


/** V1 */
class ConsumeSequence(sequence : Sequence) {
  var tick : Long = 0
  var chordBuilder = ChordBuilder()
  var song = ArrayBuffer.empty[Chord]
  var trackCollector = new TrackCollector
  val channelInfos = (0 until 16).map((i) => ChannelInfo(i))

  def output = {
    for (chord <- song) {
      chord.output
    }
  }

  private def updateChord(event : MidiEvent) : Unit = {
      event.getMessage match {
        case e:ShortMessage =>
          e.getCommand match {
            case ShortMessage.PROGRAM_CHANGE =>
              channelInfos(e.getChannel).patch = Patch(e.getData1)
            case _ =>
              chordBuilder += (e, channelInfos(e.getChannel))
          }
        case _ =>
          // Don't know how to deal with Sysex yet.
          ()
      }
  }

  private def updateChord(events : collection.mutable.Set[MidiEvent]) : Unit = {
    for(event <- events) {
      updateChord(event)
    }
    // printf("Current chord: %s\n", chord.toString)
    song += chordBuilder.result
  }

  private def gatherEventsAndUpdateChord : collection.mutable.Set[MidiEvent] = {
    return null;
  }

  /** Consume all tracks together. */
  def consume = {
    var eventsLeft = true
    while (eventsLeft) {
      val events = gatherEventsAndUpdateChord
      if (events.size == 0) {
        eventsLeft = false
      }
    }
  }

  def consumeTracksIndependently = {
    for (track <- sequence.getTracks) {
      chordBuilder = ChordBuilder()
      var time = track.get(0).getTick
      for (i <- 0 until track.size) {
	val event = track.get(i)
	updateChord(event)
	// printf("%d -> %d : %s\n", time, event.getTick, chordBuilder.toString)
	if (!chordBuilder.isEmpty && event.getTick > time + 1) { // Allow +- 1
	  song += chordBuilder.result
	  time = event.getTick
	}
      }
    }
  }

  class TrackEventIterator(val trackNum : Int, val track : Track) {
    var index = 0

    def currentEvent : Option[MidiEvent] = {
      if (index < track.size) {
	Some(track.get(index))
      } else {
	None
      }
    }

    def advance = index += 1
  }

  class TrackCollector {
    val trackIterators =
	sequence.getTracks.zipWithIndex.map
	    { case (track, i) => new TrackEventIterator(i, track) }


    def nextEvents : collection.mutable.Set[MidiEvent] = {
      var events = collection.mutable.Set.empty[MidiEvent]
      var minTick : Long = -1
      for (trackIterator <- trackIterators) {
	var anyEventsAdded = false
	// printf("\tOn track %d\n", trackNum)
	do {
	  anyEventsAdded = false
	  for (event <- trackIterator.currentEvent) {
	    if (event.getTick < tick) {
	      // Bad thing.
	      printf("Warning: track %d event %d lower than expected tick %d vs %d\n",
		trackIterator.trackNum,
		trackIterator.index,
		event.getTick,
		tick)
	    } else if (event.getTick <= tick + 1) {  // Allow +1 tick events through.
	      printf("\t\tAdding event at: %d\n", event.getTick)
	      events += event
	      // advanceTrackEvent(trackNum)
	      anyEventsAdded = true
	    } else {
	      // printf("\t\tLeaving event at: %d\n", event.getTick)
	      if (minTick == -1 || event.getTick < minTick) {
		minTick = event.getTick
	      }
	    }
	  }
	} while (anyEventsAdded)
      }
      // printf("Gathering events at %d\n", tick)
      if (events.size == 0 && minTick == tick && tick > 0) {
	// printf("Warning: no events added at tick: %d\n", tick)
	System.exit(1)
      } /* else {
        printf("Tick: %d, got %d events\n", tick, events.size)
      } */
      tick = minTick
      return events
    }
  }
}
