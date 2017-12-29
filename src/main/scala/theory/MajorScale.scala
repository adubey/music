package ca.dubey.music.theory

object MajorScale {
  val intervals : Array[Int] = Array(0, 2, 4, 5, 7, 9, 11)
}

class MajorScale(val baseKey : Int) extends Scale {
  val intervals = MajorScale.intervals
  val notes : Array[Key] =
      MajorScale.intervals.map((x) => new Key(x + baseKey))

  override def toString = notes.map((x) => x.toString).mkString(" ")
}
