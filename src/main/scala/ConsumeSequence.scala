package ca.dubey.music

import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiChannel
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import scala.collection.mutable.ArrayBuffer

class ConsumeSequence(sequence : Sequence) {
  var tick : Long = 0
  val tracks = sequence.getTracks
  var trackEvent = Array.fill[Int](tracks.size)(0)
  val channelStates = (0 until 16).map((i) => ChannelState(i))
  val variance = 1
  var chord = Chord()
  var song = ArrayBuffer.empty[Chord]

  def play(mc : MidiChannel) = {
    for (chord <- song) {
      chord.play(mc)
    }
  }

  def output = {
    for (chord <- song) {
      chord.output
    }
  }

  def currentTrackEvent(track : Int) : Option[MidiEvent] = {
    val index = trackEvent(track)
    if (index < tracks(track).size) {
      Some(tracks(track).get(index))
    } else {
      None
    }
  }

  def advanceTrackEvent(track : Int) = trackEvent(track) += 1

  def updateChord(events : collection.mutable.Set[MidiEvent]) : Unit = {
    chord = Chord(chord)  // Clone.
    for(event <- events) {
      event.getMessage match {
        case e:ShortMessage =>
          e.getCommand match {
            case ShortMessage.PROGRAM_CHANGE =>
              channelStates(e.getChannel).patch = MidiPatch(e.getData1)
            case _ =>
              chord.updateChordWithShortMessage(e, channelStates(e.getChannel))
          }
        case _ =>
          // Don't know how to deal with Sysex yet.
          ()
      }
    }
    // printf("Current chord: %s\n", chord.toString)
    song += Chord(chord)
  }

  def gatherEvents : collection.mutable.Set[MidiEvent] = {
    var events = collection.mutable.Set.empty[MidiEvent]
    var minTick : Long = -1
    // printf("Gathering events at %d\n", tick)
    for (trackNum <- 0 until tracks.size) {
      var anyEventsAdded = false
      // printf("\tOn track %d\n", trackNum)
      do {
        anyEventsAdded = false
        for (event <- currentTrackEvent(trackNum)) {
          if (event.getTick < tick) {
            // Bad thing.
            printf("Warning: track %d event %d lower than expected tick %d vs %d\n",
              trackNum, trackEvent(trackNum), event.getTick, tick)
          } else if (event.getTick <= tick + 1) {  // Allow +1 tick events through.
            printf("\t\tAdding event at: %d\n", event.getTick)
            events += event
            advanceTrackEvent(trackNum)
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
    if (events.size == 0 && minTick == tick && tick > 0) {
      // printf("Warning: no events added at tick: %d\n", tick)
      System.exit(1)
      } /* else {
        printf("Tick: %d, got %d events\n", tick, events.size)
      } */
     tick = minTick
     updateChord(events)
     return events
  }

  def consume = {
    var eventsLeft = true
    while (eventsLeft) {
      val events = gatherEvents
      if (events.size == 0) {
        eventsLeft = false
      }
    }
  }

}
