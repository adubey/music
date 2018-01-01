package ca.dubey.music.percussion

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.File
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.NoteEvent
import ca.dubey.music.midi.event.NoteOff
import ca.dubey.music.midi.event.NoteOn
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import ca.dubey.music.theory.TimeSignature
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import collection.mutable.Set
import javax.sound.midi.Sequence
import javax.sound.midi.MidiEvent
import javax.sound.midi.Track

object ExtractMeasures extends App {
  val letters = ('A'.toInt to 'Z'.toInt).map((i:Int) => i.toChar.toString).toArray

  for (filename <- args) {
    try {
      for (sequence <- File.loadSequence(filename)) {
        val consumer = new ExtractMeasures(sequence)
        printf("Song: %s\n", filename)
        for (track <- sequence.getTracks) {
          consumer.consume(track)
        }
      }
    } catch {
      case e:Exception => 
        printf("Skipped %s due to %s\n", filename, e)
    }
  }
}

class ExtractMeasures(sequence : Sequence) extends EventConsumer {
  val channelInfos = (0 until 16).map((i) => ChannelInfo(i))
  var time : Long = 0
  var ticksPerMeasure : Long = 0
  var normalizedTicksPerMeasure : Long = 0
  var tempo : TempoEvent = null
  var timeSignature : TimeSignature = null
  var ons = 0
  var wasExtracted = false
  var notesInMeasure = ArrayBuffer.empty[NoteEvent]
  var measureNum = 0
  var ppq = sequence.getResolution

  override def init(track : Track) = {
    time = 0
    tempo = null
    timeSignature = null
    printf("Info Timing %f %d\n", sequence.getDivisionType, sequence.getResolution)
  }

  def newMeasure(tick : Long) = {
    measureNum = (tick / ticksPerMeasure).toInt
    notesInMeasure = ArrayBuffer.empty[NoteEvent]
  }

  def outputMeasure : Unit = {
    val noteCount = HashMap.empty[Int, Int]
    val keysInMeasure = Set.empty[Int]
    val unmatchedNoteOns = Set.empty[Int]
    val noteOffsInMeasure = Set.empty[Int]

    for (note <- notesInMeasure) {
      note match {
        case on:NoteOn =>
          noteCount.update(on.key, 1 + noteCount.getOrElseUpdate(on.key, 0))
          unmatchedNoteOns += on.key
          keysInMeasure += on.key
        case off:NoteOff =>
          unmatchedNoteOns -= off.key
        case _ => ()
      }
    }

    if (keysInMeasure.size > ExtractMeasures.letters.size) {
      printf("Info Skipping: too many keys %d\n", keysInMeasure.size)
      return
    }
    if (unmatchedNoteOns.size > 0) {
      printf("Info Skipping: %d unmatched note ons\n", unmatchedNoteOns.size)
      return
    }

    var letterNum = 0
    val keyToString = HashMap.empty[Int, String]
    for ((key,count) <- noteCount.toSeq.sortBy(_._2).reverse) {
      val letter = ExtractMeasures.letters(letterNum)
      keyToString.update(key, letter)
      printf("Map %d %s %d\n", key, letter, count)
      letterNum += 1
    }

    var measureOut = List.newBuilder[String]
    for (note <- notesInMeasure) {
      note match {
        case on:NoteOn =>
          measureOut += "+ %s %d".format(
              keyToString(on.key),
              on.tick % normalizedTicksPerMeasure)
        case off:NoteOff =>
          measureOut += "- %s %d".format(
              keyToString(off.key),
              off.tick % normalizedTicksPerMeasure)
        case _ => ()
      }
    }
    printf("Measure %s\n", measureOut.result.mkString(" "))
  }

  def checkMeasure(tick : Long) = {
    if (tick / ticksPerMeasure > measureNum) {
      outputMeasure
      newMeasure(tick)
    }
  }

  override def tempo(track : Track, event : MidiEvent, tempoEvent : TempoEvent) {
    tempo = tempoEvent
    printf("Info %s\n", tempo)
    wasExtracted = true
  }

  override def timeSignature(track : Track, event : MidiEvent, timeSignatureEvent : TimeSignatureEvent) {
    timeSignature = timeSignatureEvent.timeSignature
    ticksPerMeasure = timeSignatureEvent.timeSignature.ticksPerMeasure(sequence.getResolution)
    normalizedTicksPerMeasure = timeSignatureEvent.timeSignature.ticksPerMeasure(Data.NORMALIZED_PPQ)
    printf("Info %s\n", timeSignatureEvent)
    wasExtracted = true
  }

  override def noteOn(track : Track, event : MidiEvent, channel : Int, key : Int, velocity : Int) = {
    if (channel == 9 && Data.isKeyInRange(key)) {
      ons += 1
      checkMeasure(event.getTick)
      notesInMeasure += NoteOn(Data.normalizeTick(event.getTick, ppq), key, velocity)
      wasExtracted = true
    }
  }

  override def noteOff(track : Track, event : MidiEvent, channel : Int, key : Int) = {
    if (channel == 9 && Data.isKeyInRange(key)) {
      ons -= 1
      checkMeasure(event.getTick)
      notesInMeasure += NoteOff(Data.normalizeTick(event.getTick, ppq), key)
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
