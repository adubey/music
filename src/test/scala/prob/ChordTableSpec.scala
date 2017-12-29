import org.scalatest.FlatSpec
import org.scalatest.PrivateMethodTester
import org.scalatest.PrivateMethodTester._
import ca.dubey.music.prob.ChordTable
import ca.dubey.music.prob.ChordTable._
import ca.dubey.music.theory.Chord

class ChordTableSpec extends FlatSpec with PrivateMethodTester {
  "A ChordTable" should "parse a line form a text file" in {
    val chord1 = ChordTable.parseLine("C | C = 0.8")
    assert(chord1 == Some((Chord("C"), Chord("C"), 0.8)))
  }

  "A ChordTable" should "get chords in decreasing probability" in {
    val chordTable = ChordTable()
    // High probability first.
    chordTable += ChordTable.parseLine("A | C = 0.2")
    chordTable += ChordTable.parseLine("C | C = 0.8")
    assert(chordTable.get(Chord("C"), 0.5) == Some(Chord("C")))
    assert(chordTable.get(Chord("C"), 0.9) == Some(Chord("A")))

    // Reverse of above. Keep same table.
    chordTable += ChordTable.parseLine("C | A = 0.2")
    chordTable += ChordTable.parseLine("A | A = 0.8")
    assert(chordTable.get(Chord("A"), 0.5) == Some(Chord("A")))
    assert(chordTable.get(Chord("A"), 0.9) == Some(Chord("C")))

    // Three notes.
    chordTable += ChordTable.parseLine("A | D = 0.25")
    chordTable += ChordTable.parseLine("B | D = 0.5")
    chordTable += ChordTable.parseLine("C | D = 0.25")

    assert(chordTable.get(Chord("D"), 0.4) == Some(Chord("B")))
    // Don't care about alphabetic ordering.
    assert(chordTable.get(Chord("D"), 0.6) == Some(Chord("A")) ||
           chordTable.get(Chord("D"), 0.6) == Some(Chord("C")))
    assert(chordTable.get(Chord("D"), 0.8) == Some(Chord("A")) ||
           chordTable.get(Chord("D"), 0.8) == Some(Chord("C")))


  }

}
