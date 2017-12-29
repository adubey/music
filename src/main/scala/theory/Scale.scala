package ca.dubey.music.theory

trait Scale {
  val baseKey : Int
  val intervals : Array[Int]
  val notes : Array[Key]
}

