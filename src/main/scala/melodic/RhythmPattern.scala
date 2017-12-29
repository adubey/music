package ca.dubey.music.melodic

import ca.dubey.music.prob.Prob

abstract class RhythmElement {
  def duration : Int
}

case class On(duration_ : Int, val offset : Int) extends RhythmElement {
  def duration : Int = duration_
}
case class Rest(duration_ : Int, val offset : Int) extends RhythmElement {
  def duration : Int = duration_
}

class RhythmPattern(val pattern : Array[RhythmElement]) {
  val duration = pattern.foldLeft(0) {
    case (d : Int, On(duration, offset)) => math.max(d, offset) + duration
    case (d : Int, Rest(duration, offset)) => math.max(d, offset) + duration
    case (d : Int, __) => d
  }

  val numStrikes : Int = pattern.foldLeft(0) {
    case (s : Int, On(_, _)) => s+1
    case (s : Int, _) => s
  }

  val numRests : Int = pattern.foldLeft(0) {
    case (r : Int, Rest(_, _)) => r+1
    case (r, _) => r
  }
}

class RhythmPatternBuilder(val ticks : Array[Int],
                           val strikeProb : Prob,
                           val restProb : Prob,
                           val strikeVsRest : Double) {
  val r = new util.Random
  def fill(numTicks : Int) : RhythmPattern = {
    var offset = 0
    var pattern = Array.newBuilder[RhythmElement]
    while (offset < numTicks) {
      var event =
        if (r.nextFloat > strikeVsRest) {
          Rest(ticks(strikeProb.sample), offset)
        } else {
          On(ticks(restProb.sample), offset)
        }
      if (event.duration + offset > numTicks) {
        if (offset - numTicks < ticks(0)) {
          // Too fast - try again.
          return fill(numTicks)
        } else {
          event = Rest(offset - numTicks, offset)
        }
      }
      pattern += event
      offset += event.duration + 3
    }
    return new RhythmPattern(pattern.result)
  }

}

object RhythmPattern {
  // quaver, crotchet, dotted crotchet, minim, semibreve
  val NOTE_TICKS = Array(12, 24, 32)
}
