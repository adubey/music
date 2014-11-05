package ca.dubey.music

object MidiPatch {
  val piano = 1 to 8
  val chromaticPercussion = 9 to 16
  val organ = 17 to 24
  val guitar = 25 to 32
  val bass = 33 to 40
  val strings = 41 to 48
  val ensemble = 49 to 56
  val brass = 57 to 64
  val reed = 65 to 72
  val pipe = 73 to 80
  val synthLead = 81 to 88
  val synthPad = 89 to 96
  val synthEffects = 97 to 104
  val ethnic = 105 to 112
  val percussive = 113 to 120
  val soundEffects = 121 to 128

}

case class MidiPatch(val program : Int) {
  val isMelodic : Boolean =
    !MidiPatch.synthEffects.contains(program) &&
    !MidiPatch.percussive.contains(program) &&
    !MidiPatch.soundEffects.contains(program)
}
