package ca.dubey.music.theory

object MajorChord {
  def intervals = Array(0, 4, 7)
}

class MajorChord(val baseKey : Int) {
  val keys : Array[Key] =
      MajorChord.intervals.map((x) => new Key(x + baseKey))

  override def toString = keys.map((x) => x.toString).mkString(" ")
}
