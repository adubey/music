package ca.dubey.music.percussion

import ca.dubey.music.midi.File
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import javax.sound.midi.Sequence
import javax.sound.midi.MidiEvent
import javax.sound.midi.Track

object Extract extends App {
  for (filename <- args) {
    for (sequence <- File.loadSequence(filename)) {
      val consumer = new Extract(sequence)
      printf("Song: %s\n", filename)
      for (track <- sequence.getTracks) {
        consumer.consume(track)
      }
    }
  }
}

class Extract(sequence : Sequence) extends EventConsumer {
  var time : Long = 0
  var tempo : TempoEvent = null
  var timeSignature : TimeSignatureEvent = null
  var wasExtracted = false

  override def init(track : Track) = {
    time = 0
    tempo = null
    timeSignature = null
    printf("Info Timing %f %d\n", sequence.getDivisionType, sequence.getResolution)
  }

  override def tempo(track : Track, event : MidiEvent, tempoEvent : TempoEvent) {
    tempo = tempoEvent
    printf("Info %s\n", tempo)
    wasExtracted = true
  }

  override def timeSignature(track : Track, event : MidiEvent, timeSignatureEvent : TimeSignatureEvent) {
    timeSignature = timeSignatureEvent
    printf("Info %s\n", timeSignatureEvent)
    wasExtracted = true
  }

  override def noteOn(track : Track, event : MidiEvent, channel : Int, key : Int, velocity : Int) = {
    if (channel == 9 && Data.isKeyInRange(key)) {
      printf("+ %d %d %d\n", event.getTick - time, key, velocity)
      wasExtracted = true
    }
  }

  override def noteOff(track : Track, event : MidiEvent, channel : Int, key : Int) = {
    if (channel == 9 && Data.isKeyInRange(key)) {
      printf("- %d %d\n", event.getTick - time, key)
      wasExtracted = true
    }
  }

  override def eventPost(track : Track, event : MidiEvent) = {
    if (wasExtracted) {
      time = event.getTick
      wasExtracted = false
    }
  }
}
