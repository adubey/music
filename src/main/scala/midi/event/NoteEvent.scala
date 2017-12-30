package ca.dubey.music.midi.event

abstract class NoteEvent {
  val tick : Long
  val key : Int
  override def toString : String = ""
}

case class NoteOn(val tick : Long, val key : Int, val velocity : Int) extends NoteEvent {
  override def toString : String = if (velocity > 0) { "N(%d,%d)".format(tick, key) } else ""
}

case class NoteOff(val tick : Long, val key : Int) extends NoteEvent

case class Skip(val tick : Long) extends NoteEvent {
  val key = 0
}
