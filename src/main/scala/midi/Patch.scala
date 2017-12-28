package ca.dubey.music.midi

/** General MIDI patches */
object Patch {
  val piano = 0 until 8
  val chromaticPercussion = 8 until 16
  val organ = 16 until 24
  val guitar = 24 until 32
  val bass = 32 until 40
  val strings = 40 until 48
  val ensemble = 48 until 56
  val brass = 56 until 64
  val reed = 64 until 72
  val pipe = 72 until 80
  val synthLead = 80 until 88
  val synthPad = 88 until 96
  val synthEffects = 96 until 104
  val ethnic = 104 until 112
  val percussive = 112 until 120
  val soundEffects = 120 until 128
}

case class Patch(val program : Int) {
  val isMelodic : Boolean =
    !Patch.synthEffects.contains(program) &&
    !Patch.percussive.contains(program) &&
    !Patch.soundEffects.contains(program)
}
