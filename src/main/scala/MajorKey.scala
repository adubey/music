package ca.dubey.music

object MajorKey {
  def offsets = Array(0, 2, 4, 5, 7, 9, 11)
}

class MajorKey(val baseNote : Int) {
  val notes : Array[Note] = MajorKey.offsets.map((x) => new Note(x + baseNote))

  override def toString = notes.map((x) => x.toString).mkString(" ")
}
