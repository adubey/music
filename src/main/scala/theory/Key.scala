package ca.dubey.music.theory

trait Key {
  val baseNote : Int
  val intervals : Array[Int]
  val notes : Array[Note]
}

