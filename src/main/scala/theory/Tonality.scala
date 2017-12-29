package ca.dubey.music.theory

object Tonality {
  val fromByte : (Byte) => Tonality = {
    case 0 => Major
    case 1 => Minor
  }
}

abstract class Tonality {
  def toByte : Byte
}
case object Major extends Tonality {
  override def toByte : Byte = 0
}
case object Minor extends Tonality {
  override def toByte : Byte = 1
}

