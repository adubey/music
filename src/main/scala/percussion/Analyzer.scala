package ca.dubey.music.percussion

import ca.dubey.music.midi.ChannelInfo
import ca.dubey.music.midi.File
import ca.dubey.music.midi.event.EventConsumer
import ca.dubey.music.midi.event.TimeSignatureEvent
import ca.dubey.music.midi.event.TempoEvent
import ca.dubey.music.midi.event.NoteEvent
import ca.dubey.music.midi.event.NoteOn
import ca.dubey.music.midi.event.NoteOff
import ca.dubey.music.midi.event.Skip
import cc.mallet.classify.ClassifierTrainer
import cc.mallet.classify.MaxEntTrainer
import cc.mallet.classify.MaxEnt
import cc.mallet.types.Alphabet
import cc.mallet.types.FeatureVector
import cc.mallet.types.Instance
import cc.mallet.types.InstanceList
import cc.mallet.types.Label
import cc.mallet.types.LabelAlphabet
import collection.mutable.ArrayBuffer
import collection.mutable.IndexedSeq
import collection.mutable.ListBuffer
import collection.mutable.Set
import collection.mutable.Queue
import io.Source
import java.io.BufferedInputStream
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.ObjectOutputStream
import java.io.FileOutputStream
import javax.sound.midi.InvalidMidiDataException
import javax.sound.midi.MidiSystem
import javax.sound.midi.Sequence
import javax.sound.midi.ShortMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.Track

class Analyzer {
  val noteOnEvent = raw"\+ (\d+) (\d+) (\d+)".r
  val noteOffEvent = raw"- (\d+) (\d+)".r
  val timeSignature = raw"Info.*Time Signature (\d+) / (\d+)".r
  val timing = raw"Info Timing ([\.\d]+) (\d+)".r
  val bpm = raw"Info.*BPM (\d+)".r

  /**
   * @param filename The filename with one event per line, in text mode.
   * @param callback The function to analyze each song.
   * @param limitOption Either None or the maximum number of lines (rounded up to the next song end)
   */
  def analyzeSongs(
      filename : String,
      callback : (NoteIterable) => Unit,
      limitOption : Option[Int] = None) : Unit = {
    var numSongs = 0
    var numLines = 0
    var notes = ArrayBuffer.empty[NoteEvent]
    var currentSong = new NoteIterable(notes)
    var numSkipped = 0
    var numEmpty = 0
    var skipSong = false

    for (lineRaw <- Source.fromFile(filename).getLines) {
      numLines += 1
      if (numLines % 50000 == 0) {
        printf("%d\n", numLines)
      }
      val line = lineRaw.trim
      if (line.startsWith("Song")) {
        numSongs += 1
        if (!skipSong) {
          if (notes.isEmpty) {
            numEmpty += 1
          } else {
            callback(currentSong)
            for (limit <- limitOption) {
              if (numLines > limit) {
                return
              }
            }
          }
        }
        skipSong = false
        notes = ArrayBuffer.empty[NoteEvent]
        currentSong = new NoteIterable(notes)
      } else {
        if (!skipSong) {
          line match {
            case noteOnEvent(tick, note, velocity) =>
              val n = note.toInt
              if (Data.isKeyInRange(n)) {
                notes += NoteOn(tick.toInt, n, velocity.toInt)
              } else {
                notes += Skip(tick.toInt)
              }
            case noteOffEvent(tick, note) =>
              val n = note.toInt
              if (Data.isKeyInRange(n)) {
                notes += NoteOff(tick.toInt, n)
              } else {
                notes += Skip(tick.toInt)
              }
            case timing(x, y) =>
              if (x.toFloat != 0.0) {
                skipSong = true
                numSkipped += 1
              } else {
                currentSong.setPpq(y.toInt)
              }
            case timeSignature(n, d) =>
              if (d != "4" && d != "8") {
                skipSong = true
                numSkipped += 1
              } else {
                currentSong.setTimeSignature(n.toInt, d.toInt)
              }
            case _ =>  ()
          }
        }
      }
    }
  }
}
