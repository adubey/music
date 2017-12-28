package ca.dubey.music.theory

object MajorKey {
  val intervals : Array[Int] = Array(0, 2, 4, 5, 7, 9, 11)
}

class MajorKey(val baseNote : Int) extends Key {
  val intervals = MajorKey.intervals
  val notes : Array[Note] =
      MajorKey.intervals.map((x) => new Note(x + baseNote))

  override def toString = notes.map((x) => x.toString).mkString(" ")
}
