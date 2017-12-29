package ca.dubey.music.theory

/** Represent a note's value (duration) */
case class NoteValue(val ticksPerQuarterNote : Int) {
  /** Quaiver */
  val eighthNote = ticksPerQuarterNote / 2

  /** Crochet */
  val quarterNote = ticksPerQuarterNote

  /** Dotted Crochet */
  val dottedQuarterNote = ticksPerQuarterNote + eighthNote

  /** Minim */
  val halfNote = ticksPerQuarterNote * 2

  /** Dotted Minim */
  val dottedHalfNote = ticksPerQuarterNote * 3

  /** Semibreve */
  val wholeNote = ticksPerQuarterNote * 4

  val values = Array(
      eighthNote,
      quarterNote,
      dottedQuarterNote,
      halfNote,
      dottedHalfNote,
      wholeNote)


}
