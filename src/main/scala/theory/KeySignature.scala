package ca.dubey.music.theory

class Tonality
case object Major extends Tonality
case object Minor extends Tonality

class KeySignature(val baseNote : Note, val tonality : Tonality)

object KeySignature extends Enumeration {
  val baseNoteFromTonalityAndNumAccidentals : (Tonality, Int) => Note = {
    case (Major, -7) => Note.fromString("Cb")
    case (Major, -6) => Note.fromString("Gb")
    case (Major, -5) => Note.fromString("Db")
    case (Major, -4) => Note.fromString("Ab")
    case (Major, -3) => Note.fromString("Eb")
    case (Major, -2) => Note.fromString("Bb")
    case (Major, -1) => Note.fromString("F")
    case (Major, 0) => Note.fromString("C")
    case (Major, 1) => Note.fromString("G")
    case (Major, 2) => Note.fromString("D")
    case (Major, 3) => Note.fromString("A")
    case (Major, 4) => Note.fromString("E")
    case (Major, 5) => Note.fromString("B")
    case (Major, 6) => Note.fromString("F#")
    case (Major, 7) => Note.fromString("C#")

    case (Minor, -7) => Note.fromString("Ab")
    case (Minor, -6) => Note.fromString("Eb")
    case (Minor, -5) => Note.fromString("Bb")
    case (Minor, -4) => Note.fromString("F")
    case (Minor, -3) => Note.fromString("C")
    case (Minor, -2) => Note.fromString("G")
    case (Minor, -1) => Note.fromString("D")
    case (Minor, 0) => Note.fromString("A")
    case (Minor, 1) => Note.fromString("E")
    case (Minor, 2) => Note.fromString("B")
    case (Minor, 3) => Note.fromString("F#")
    case (Minor, 4) => Note.fromString("C#")
    case (Minor, 5) => Note.fromString("D#")
    case (Minor, 6) => Note.fromString("G#")
    case (Minor, 7) => Note.fromString("A#")
  }

  def fromTonalityAndNumAccidentals(tonality : Tonality, numAccidentals : Int) = {
    new KeySignature(baseNoteFromTonalityAndNumAccidentals(tonality, numAccidentals), tonality)
  }
}
