package ca.dubey.music

object Shrink {

  val joint = collection.mutable.Map.empty[(Chord,Chord), Double]
  val marginal = collection.mutable.Map.empty[Chord, Double]

  def main(args : Array[String]) : Unit = {
    var lineNum : Long = 0

    // Go once through to count marginal.
    for (arg <- args) {
      for (line <- scala.io.Source.fromFile(arg).getLines) {
        val chord = Chord(line)
        val marginalCount = marginal.getOrElse(chord, 0.0)
        marginal.put(chord, marginalCount + 1)
        lineNum += 1
        if (lineNum % 10000 == 0) {
          printf("Loading: %d %s\n", lineNum, chord)
        }
      }
    }

    // Go second time to count joint.
    lineNum = 0
    for (arg <- args) {
      var prevChord = Chord()
      for (line <- scala.io.Source.fromFile(arg).getLines) {
        val chord = Chord(line)
        val marginalCount = marginal.getOrElse(prevChord, 0.0)
        if (marginalCount > 0) {
          val jointCount = joint.getOrElse((chord,prevChord), 0.0)
          joint.put((chord,prevChord), jointCount + 1)
        }
        lineNum += 1
        if (lineNum % 10000 == 0) {
          printf("Loading: %d %s | %s\n", lineNum, chord, prevChord)
        }
        prevChord = chord
      }
    }

    for ((k,v) <- joint) {
      val count = marginal.getOrElse(k._2, 0.0)
      if (count > 0) {
        printf("%s | %s = %f\n", k._1, k._2, joint.getOrElse(k, 0.0) / count)
      }
    }
  }
}
