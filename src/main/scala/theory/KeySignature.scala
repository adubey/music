package ca.dubey.music.theory

class KeySignature(val baseKey : Key, val tonality : Tonality) {
  def numAccidentals : Int = ((tonality, baseKey.toString)) match {
    case (Major, "Cb") => -7
    case (Major, "Gb") => -6
    case (Major, "Db") => -5
    case (Major, "Ab") => -4
    case (Major, "Eb") => -3
    case (Major, "Bb") => -2
    case (Major, "F") => -1
    case (Major, "C") => 0
    case (Major, "G") => 1
    case (Major, "D") => 2
    case (Major, "A") => 3
    case (Major, "E") => 4
    case (Major, "B") => 5
    case (Major, "F#") => 6
    case (Major, "C#") => 7

    case (Minor, "Ab") => -7
    case (Minor, "Eb") => -6
    case (Minor, "Bb") => -5
    case (Minor, "F") => -4
    case (Minor, "C") => -3
    case (Minor, "G") => -2
    case (Minor, "D") => -1
    case (Minor, "A") => 0
    case (Minor, "E") => 1
    case (Minor, "B") => 2
    case (Minor, "F#") => 3
    case (Minor, "C#") => 4
    case (Minor, "D#") => 5
    case (Minor, "G#") => 6
    case (Minor, "A#") => 7
  }
}

object KeySignature extends Enumeration {
  def apply(k : Key, t : Tonality) = new KeySignature(k, t)

  val baseKeyFromTonalityAndNumAccidentals : (Tonality, Byte) => Key = {
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

  def fromTonalityAndNumAccidentals(
      tonality : Tonality, numAccidentals : Byte) = {
    new KeySignature(
        baseKeyFromTonalityAndNumAccidentals(
            tonality, numAccidentals),
        tonality)
  }
}
