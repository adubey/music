package ca.dubey.music.theory

class Tonality
case object Major extends Tonality
case object Minor extends Tonality

class KeySignature(val baseKey : Key, val tonality : Tonality)

object KeySignature extends Enumeration {
  val baseKeyFromTonalityAndNumAccidentals : (Tonality, Int) => Key = {
    case (Major, -7) => Key.fromString("Cb")
    case (Major, -6) => Key.fromString("Gb")
    case (Major, -5) => Key.fromString("Db")
    case (Major, -4) => Key.fromString("Ab")
    case (Major, -3) => Key.fromString("Eb")
    case (Major, -2) => Key.fromString("Bb")
    case (Major, -1) => Key.fromString("F")
    case (Major, 0) => Key.fromString("C")
    case (Major, 1) => Key.fromString("G")
    case (Major, 2) => Key.fromString("D")
    case (Major, 3) => Key.fromString("A")
    case (Major, 4) => Key.fromString("E")
    case (Major, 5) => Key.fromString("B")
    case (Major, 6) => Key.fromString("F#")
    case (Major, 7) => Key.fromString("C#")

    case (Minor, -7) => Key.fromString("Ab")
    case (Minor, -6) => Key.fromString("Eb")
    case (Minor, -5) => Key.fromString("Bb")
    case (Minor, -4) => Key.fromString("F")
    case (Minor, -3) => Key.fromString("C")
    case (Minor, -2) => Key.fromString("G")
    case (Minor, -1) => Key.fromString("D")
    case (Minor, 0) => Key.fromString("A")
    case (Minor, 1) => Key.fromString("E")
    case (Minor, 2) => Key.fromString("B")
    case (Minor, 3) => Key.fromString("F#")
    case (Minor, 4) => Key.fromString("C#")
    case (Minor, 5) => Key.fromString("D#")
    case (Minor, 6) => Key.fromString("G#")
    case (Minor, 7) => Key.fromString("A#")
  }

  def fromTonalityAndNumAccidentals(tonality : Tonality, numAccidentals : Int) = {
    new KeySignature(baseKeyFromTonalityAndNumAccidentals(tonality, numAccidentals), tonality)
  }
}
