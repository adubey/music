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
import collection.mutable.ListBuffer
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

object Analyzer extends App {
  val eventsFile = args(0)
  /*
  val outputFile = args(1)
  val maxLength = 4
  */

  def loadInstances(filename : String) : Analyzer = {
    val alphabet = Data.makeAlphabet
    val data = new Analyzer(Data.makeAlphabet)
    data.loadInstances(filename)
    data
  }

  val data = loadInstances(eventsFile)

  /*
  val trainer = new MaxEntTrainer()
  val classifier = trainer.train(data.instanceList)

  val ois = new ObjectOutputStream (new FileOutputStream(outputFile));
  ois.writeObject(classifier)
  ois.close();
  */
}

class Analyzer(val alphabet : LabelAlphabet) {
  val noteOnEvent = raw"\+ (\d+) (\d+) (\d+)".r
  val noteOffEvent = raw"- (\d+) (\d+)".r
  val timeSignature = raw"Info.*Time Signature (\d+) / (\d+)".r
  val timing = raw"Info Timing ([\.\d]+) (\d+)".r
  val bpm = raw"Info.*BPM (\d+)".r
  val instanceList = new InstanceList(alphabet, alphabet)
  val q = new Queue[Label]

  abstract class NoteEvent {
    val tick : Int
    val note : Int
  }
  case class NoteOn(val tick : Int, val note : Int, val velocity : Int) extends NoteEvent
  case class NoteOff(val tick : Int, val note : Int) extends NoteEvent
  case class Skip(val tick : Int) extends NoteEvent {
    val note = 0
  }

  case class Song(var notes : ArrayBuffer[NoteEvent]) {

    var ppq : Int = 24
    var n : Int = 4
    var d : Int = 4
    var beat : Int = 24
    var pulse : Int = 96

    def setPpq(ppq_ : Int) = {
      ppq = ppq_
      updateTimes
    }

    def setTimeSignature(n_ : Int, d_ : Int) = {
      n = n_
      d = d_
      updateTimes
    }

    def updateTimes = {
      beat = ppq / (d/4)
      pulse = beat * n
    }

    def countElementsWithSameDifference(target : Long, last : Long, ts : List[Long]) : Int = {
      ts match {
        case Nil => 0
        case t::ts => if (math.abs(last-t-target) < (beat/8) ) { 1+countElementsWithSameDifference(target, t, ts) } else { 0 }
      }
    }

    def analyze = {
      var last = Array.fill[List[Long]](256)(Nil)
      var time : Long = 0
      for (i <- 0 until notes.size) {
        val note = notes(i)
        time += note.tick
        val tick = note.tick.toDouble
        val norm = ppq.toDouble

        var sinceLast : Long = 0
        var repetitions : Int = 0

        last(note.note) match {
          case Nil =>
            sinceLast = -ppq
            repetitions = 0
          case t::ts if (t > pulse * 2) =>
            sinceLast = -ppq
            repetitions = 0
          case t::ts =>
            sinceLast = time - t
            repetitions = countElementsWithSameDifference(sinceLast, t, ts)
        }

        var id = " "

        note match {
          case on:NoteOn =>
            id = "+"
            last(on.note) = time :: last(on.note)
          case off:NoteOff =>
            id = "-"
          case s:Skip =>
            id = "/"
        }
        var onPulse = (time % pulse) < (beat / 8)
        val d = note.tick * 960 / ppq
        val l = sinceLast * 960L / ppq
        val b = (time % beat) * 960 / ppq
        if (id != "/") {
          printf("%s K=%d D=%d L=%d P=%b B=%d R=%d\n", id, note.note, d, l, onPulse, b, repetitions)
        }

        note match {
          case on:NoteOn =>
          case _ => ()
        }
      }
    }
  }

  def loadInstances(filename : String) : Unit = {
    var numSongs = 0
    var numLines = 0
    var currentSong = Song(ArrayBuffer.empty[NoteEvent])
    var numSkipped = 0
    var numEmpty = 0
    var skipSong = false

    for (lineRaw <- Source.fromFile(filename).getLines) {
      numLines += 1
      val line = lineRaw.trim
      if (line.startsWith("Song")) {
        numSongs += 1
        if (!skipSong) {
          if (currentSong.notes.isEmpty) {
            numEmpty += 1
          } else {
            currentSong.analyze
          }
        }
        skipSong = false
        currentSong = Song(ArrayBuffer.empty[NoteEvent])
      } else {
        if (!skipSong) {
          line match {
            case noteOnEvent(tick, note, velocity) =>
              val n = note.toInt
              if (Data.isKeyInRange(n)) {
                currentSong.notes += NoteOn(tick.toInt, n, velocity.toInt)
              } else {
                currentSong.notes += Skip(tick.toInt)
              }
            case noteOffEvent(tick, note) =>
              val n = note.toInt
              if (Data.isKeyInRange(n)) {
                currentSong.notes += NoteOff(tick.toInt, n)
              } else {
                currentSong.notes += Skip(tick.toInt)
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
