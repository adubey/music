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

object Analyzer extends App {
  val eventsFile = args(0)

  def loadInstances(filename : String) : Analyzer = {
    val data = new Analyzer
    data.loadInstances(filename)
    data
  }

  val data = loadInstances(eventsFile)

  object Output {
    val eventOld = raw"([+-]) K=(\d+) D=(\d+) L=(-?\d+) P=(\w+) B=(\d+)".r
    val eventNew = raw"([+-]) K=(\d+) D=(\d+) L=(-?\d+) P=(\w+) B=(\d+) N=(\d+) I=(\d+)".r

    def fromString(s : String) = {
      s match {
        case eventOld(eventType, key, deltaString, lastString, pulse, beatString) =>
          Output(
              eventType, key.toInt, deltaString.toInt, lastString.toInt,
              pulse match { case "true" => true case _ => false },
              beatString.toInt, 0, 0)
        case eventNew(eventType, key, deltaString, lastString, pulse, beatString, n, i) =>
          Output(
              eventType, key.toInt, deltaString.toInt, lastString.toInt,
              pulse match { case "true" => true case _ => false },
              beatString.toInt, n.toInt, i.toInt)
      }
    }
  }

  case class Output(
      val id : String,
      val key : Int,
      val deltaTime : Int,
      val lastOccurrence :  Int,
      val onPulse : Boolean,
      val timeSinceBeat : Int,
      val numThisKeyInMeasure : Int,
      val numKeysInMeasure : Int) {

    override def toString : String = {
      return "%s K=%d D=%d L=%d P=%b B=%d N=%d I=%d".format(
          id, key, deltaTime, lastOccurrence, onPulse, timeSinceBeat,
          numThisKeyInMeasure, numKeysInMeasure)
    }

    def encode(dataAlphabet : LabelAlphabet, targetAlphabet : LabelAlphabet) : Instance = {
      val delta = Data.deltaQuantizer.quantize(deltaTime)
      val last = Data.lastNoteQuantizer.quantize(lastOccurrence)
      val beat = Data.beatQuantizer.quantize(timeSinceBeat)

      val lastLabel =
          dataAlphabet.lookupLabel("L_%s".format(last))
      val beatLabel =
          dataAlphabet.lookupLabel("B_%s".format(beat))
      val pulseLabel =
          dataAlphabet.lookupLabel(onPulse.toString)

      return new Instance(
          new FeatureVector(
              dataAlphabet,
              Array(lastLabel.getIndex, beatLabel.getIndex, pulseLabel.getIndex)),
          targetAlphabet.lookupLabel("%s_%d_%d".format(id, key, delta)),
          "",
          "")
    }
  }

  case class Song(var notes : IndexedSeq[NoteEvent]) {

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

    class SongAnalyzer {
      var last = Array.fill[Long](-ppq)(Nil)
      var numInMeasure = Array.fill[Int](256)(0)
      var instrumentsInMeasure = Set.empty[Int]
      var time : Long = 0
      var notesSincePulse : Int = 0

      def analyzeWithoutAdvancing(note : NoteEvent) : Option[Output] = {
        val newTime = time + note.tick
        if (newTime/pulse > time/pulse) {
          notesSincePulse = 0
          for (i <- instrumentsInMeasure) {
            numInMeasure(i) = 0
          }
          instrumentsInMeasure = Set.empty[Int]
        }
        val tick = note.tick.toDouble
        val norm = ppq.toDouble

        var sinceLast : Long = 0
        var repetitions : Int = 0

        last(note.key) match {
          case Nil =>
            sinceLast = -ppq
            repetitions = 0
          case t::ts if (t > pulse * 2) =>
            sinceLast = -ppq
            repetitions = 0
          case t::ts =>
            sinceLast = newTime - t
            repetitions = countElementsWithSameDifference(sinceLast, t, ts)
        }

        var onPulse = (newTime % pulse) < (beat / 8)
        val d = note.tick * 960 / ppq
        val l = sinceLast * 960L / ppq
        val b = (newTime % beat) * 960 / ppq
        var result = Option.empty[Output]
        if (note.id != "/") {
          result = Some(Output(
              note.id, note.key, d.toInt, l.toInt, onPulse, b.toInt,
              numInMeasure(note.key), instrumentsInMeasure.size))
        }
        return result
      }

      def advance(note : NoteEvent) = {
        note match {
          case on:NoteOn if (on.velocity > 0) =>
            last(on.key) = time :: last(on.key)
            notesSincePulse += 1
            numInMeasure(on.key) = numInMeasure(on.key) + 1
            instrumentsInMeasure += on.key
          case _ => ()
        }
      }

      def analyze(note : NoteEvent) : Option[Output] = {
        val result = analyzeWithoutAdvancing(note)
        advance(note)
        return result
      }
    }

    def analyze = {
      val songAnalyzer = new SongAnalyzer
      for (note <- notes) {
        for (output <- songAnalyzer.analyze(note)) {
          printf("%s\n", output)
        }
      }
    }
  }
}


class Analyzer {
  val noteOnEvent = raw"\+ (\d+) (\d+) (\d+)".r
  val noteOffEvent = raw"- (\d+) (\d+)".r
  val timeSignature = raw"Info.*Time Signature (\d+) / (\d+)".r
  val timing = raw"Info Timing ([\.\d]+) (\d+)".r
  val bpm = raw"Info.*BPM (\d+)".r


  def loadInstances(filename : String) : Unit = {
    var numSongs = 0
    var numLines = 0
    var notes = ArrayBuffer.empty[NoteEvent]
    var currentSong = Analyzer.Song(notes)
    var numSkipped = 0
    var numEmpty = 0
    var skipSong = false

    for (lineRaw <- Source.fromFile(filename).getLines) {
      numLines += 1
      val line = lineRaw.trim
      if (line.startsWith("Song")) {
        numSongs += 1
        if (!skipSong) {
          if (notes.isEmpty) {
            numEmpty += 1
          } else {
            currentSong.analyze
          }
        }
        skipSong = false
        notes = ArrayBuffer.empty[NoteEvent]
        currentSong = Analyzer.Song(notes)
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
