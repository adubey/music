package ca.dubey.music.theory

object MajorChord {
  def intervals = Array(0, 4, 7)
}

class MajorChord(val baseNote : Int) {
  val notes : Array[Note] =
      MajorChord.intervals.map((x) => new Note(x + baseNote))

  override def toString = notes.map((x) => x.toString).mkString(" ")
}
