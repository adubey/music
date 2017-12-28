package ca.dubey.music

import ca.dubey.music.prob.ChordTable
import ca.dubey.music.theory.Chord
import ca.dubey.music.theory.MajorKey
import ca.dubey.music.theory.Note

class MelodyPattern(val pattern : Array[Chord]) {
  def numStrikes = pattern.size
}

/**
 * @param{chordTable} A probability distribution over chord changes.
 */
class MelodyPatternBuilder(val chordTable : ChordTable) {
  val r = new util.Random

  def fill(r : RhythmPattern) : MelodyPattern = {
    val elements = Array.newBuilder[Chord]
    for (i <- 1 to r.numStrikes) {
      val chordOption = chordTable.sample
      for (chord <- chordOption) {
	elements += chord
      }
    }
    val result = elements.result
    if (result.size < r.numStrikes) {
      // Try again.
      return fill(r)
    }
    return new MelodyPattern(result)
  }
}
