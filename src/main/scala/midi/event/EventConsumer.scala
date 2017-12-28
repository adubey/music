package ca.dubey.music.midi.event

import javax.sound.midi.Track
import javax.sound.midi.MidiEvent
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage

class EventConsumer {

  def init(track : Track) = ()
  def eventPre(track: Track, event : MidiEvent) = ()
  def noteOn(track : Track, event : MidiEvent, channel : Int, note : Int, velocity : Int) = ()
  def noteOff(track : Track, event : MidiEvent, channel : Int, note : Int) = ()
  def tempo(track : Track, event : MidiEvent, tempo : TempoEvent) = ()
  def keySignature(track : Track, event : MidiEvent, keySignature : KeySignatureEvent) = ()
  def timeSignature(track : Track, event : MidiEvent, timeSignature : TimeSignatureEvent) = ()
  def endTrack(track : Track, event : MidiEvent) = ()
  def eventPost(track: Track, event : MidiEvent) = ()
  def cleanup(track : Track) = ()

  def consume(track : Track) = {
    init(track)
    for (i <- 0 until track.size) {
      var event = track.get(i)
      eventPre(track, event)
      event.getMessage match {
        case e:ShortMessage =>
          e.getCommand match {
            case ShortMessage.NOTE_ON =>
              if (e.getData2 == 0) {
                noteOff(track, event, e.getChannel, e.getData1)
              } else {
                noteOn(track, event, e.getChannel, e.getData1, e.getData2)
              }
            case ShortMessage.NOTE_OFF => noteOff(track, event, e.getChannel, e.getData1)
            case _ => ()
          }
        case e:MetaMessage =>
          e.getType match {
            case EndTrackEvent.EVENT_ID => endTrack(track, event)
            case TempoEvent.EVENT_ID =>
              tempo(track, event, TempoEvent.fromMidiEventData(e.getData).get)
            case KeySignatureEvent.EVENT_ID =>
              keySignature(track, event, KeySignatureEvent.fromMidiEventData(e.getData).get)
            case TimeSignatureEvent.EVENT_ID =>
              timeSignature(track, event, TimeSignatureEvent.fromMidiEventData(e.getData).get)
            case _ => ()
          }
        case _ => () // Sysex message.
      }
      eventPost(track, event)
    }
    cleanup(track)
  }
}
