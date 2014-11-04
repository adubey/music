package ca.dubey.music

import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue

object ChordTable {
  def parseLine(line : String) : Option[(Chord, Chord, Double)] = {
    // Remove equals.
    val chordsProb = line.split(" = ")
    if (chordsProb.size != 2) {
      printf("= %d\n", chordsProb.size)
      println
      return None
    }
    val chords = chordsProb(0).split(" \\| ")
    if (chords.size != 2) {
      printf("| %d\n", chords.size)
      println
      return None
    }
    Some((Chord.fromString(chords(0)),
	  Chord.fromString(chords(1)),
	  chordsProb(1).toDouble))
  }

  def fromFile(filename : String) : ChordTable = {
    val table = apply()
    for (line <- scala.io.Source.fromFile(filename).getLines) {
      parseLine(line) match {
	case Some((c1, c2, p)) =>
	  table += (c1, c2, p)
	case _ =>
	  printf("Warning can't parse: %s\n", line)
      }
    }
    return table
  }

  def apply() : ChordTable = {
    return new ChordTable(Map.empty[Chord, PriorityQueue[ChordProb]])
  }
}

case class ChordProb(val chord : Chord,
		     val prob : Double) extends Ordered[ChordProb] {
  def compare(that : ChordProb) = prob.compare(that.prob)
}

class ChordTable(val table : Map[Chord, PriorityQueue[ChordProb]]) {

  def +=(option : Option[(Chord, Chord, Double)]) : Unit = {
    for (x <- option) {
      +=(x._1, x._2, x._3)
    }
  }

  def +=(c1 : Chord, c2 : Chord, p : Double) : Unit = {
    val subtable = table.getOrElse(c2, PriorityQueue.empty[ChordProb])
    subtable += ChordProb(c1, p)
    table.put(c2, subtable)
  }

  val r = new scala.util.Random

  def get(given : Chord, p : Double) : Option[Chord] = {
    val subtable = table.getOrElse(given, null)
    if (subtable == null) {
      return None
    }
    var currentP = 0.0
    var lastChord = Option.empty[Chord]
    for (chord <- subtable) {
      if (currentP >= p) {  // P = 0 always returns None.
	return lastChord
      }
      lastChord = Some(chord.chord)
      currentP += chord.prob
    }
    return lastChord
  }

  def sample(given : Chord) : Option[Chord] = {
    val p = r.nextDouble
    get(given, p)
  }
}
