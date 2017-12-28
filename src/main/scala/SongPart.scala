package ca.dubey.music

import ca.dubey.music.theory.Chord
import ca.dubey.music.theory.Note
import javax.sound.midi.Track
import javax.sound.midi.ShortMessage
import javax.sound.midi.MidiEvent

case class SongElement(val r : RhythmElement, val m : Chord)

object SongPart {
  def mix(start : Int, r : RhythmPattern, m : MelodyPattern) : SongPart = {
    assert(r.numStrikes == m.numStrikes)
    val parts = Array.newBuilder[SongElement]
    val im = m.pattern.iterator

    for (s <- r.pattern) {
      s match {
        case On(_, _) => parts += SongElement(s, im.next)
        case Rest(_, _) => ()
      }
    }
    return new SongPart(start, parts.result)
  }
}

class SongPart(
    val start : Int,
    val parts : Array[SongElement])
        extends ca.dubey.music.midi.TrackBuilder {
  val duration : Int = parts.foldLeft(0) {
    case (d : Int, SongElement(On(duration, offset), _)) =>
      math.max(d, offset) + duration
    case (d : Int, SongElement(Rest(duration, offset), _)) =>
      math.max(d, offset) + duration
  }

  private def addChord(t : Track,
		       c : Chord,
		       duration : Int,
		       time : Int) : Unit = {
    for (note <- c.simplify.notes) {
      addNote(t, note.value, duration, time)
    }
  }

  def addToTrack(t : Track) = {
    for (part <- parts) {
      part match {
        case SongElement(On(duration, offset), chord) =>
          addChord(t, chord, duration, offset + start)
        case _ => ()
      }
    }
  }
}
