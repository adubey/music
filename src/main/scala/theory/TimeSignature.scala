package ca.dubey.music.theory

class TimeSignature(val beatsPerMeasure : Int,
                    val beatUnit : Int) {
  def ticksPerMeasure(ticksPerQuarterNote : Int) : Int = {
    return ticksPerQuarterNote / (beatUnit/4) * beatsPerMeasure
  }
}
