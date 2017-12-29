package ca.dubey.music

import ca.dubey.music.prob.ChordTable
import ca.dubey.music.prob.Prob
import ca.dubey.music.theory.NoteValue
import ca.dubey.music.midi.Patch
import ca.dubey.music.midi.SequencePlayer
import ca.dubey.music.midi.event.TempoEvent
import javax.sound.midi.Sequence
import javax.sound.midi.Sequencer
import javax.sound.midi.Track
import javax.sound.midi.ShortMessage
import javax.sound.midi.MetaMessage
import javax.sound.midi.MidiEvent

class TrackBuilder(
    val ticksPerBeat : Int,
    val tempo : Int,
    chordTable : ChordTable)
        extends SequencePlayer with ca.dubey.music.midi.TrackBuilder {
  protected val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
  protected val t = sequence.createTrack

  def build : Track = {
    val r = new util.Random
    // 8 measures.
    val duration = ticksPerBeat * 4 * 2
    val rhythmBuilder = new RhythmPatternBuilder(
      NoteValue(ticksPerBeat).values,
      new Prob(Array(0.25, 0.5, 0.2, 0.04, 0.03, 0.01)),
      new Prob(Array(0.5, 0.4, 0.1, 0.0, 0.0, 0.0)),
      0.8
      )
    val rs = rhythmBuilder.fill(duration)
    val melodyBuilder = new MelodyPatternBuilder(chordTable)
    val ms = melodyBuilder.fill(rs)

    var time = 0

    addNameEvent(t, "some track")
    addPolyOn(t)
    addProgramChange(t, Patch.piano(0))

    for (i <- 1 to 20) {
      val rh =
        if (i % 2 == 0) {
          rs
        } else {
          rhythmBuilder.fill(duration)
        }
      val m =
        if (rh.numStrikes == rs.numStrikes && r.nextFloat > 0.4) {
          ms
        } else {
          melodyBuilder.fill(rh)
        }
      val sp = SongPart.mix(time, rh, m)
      sp.addToTrack(t)
      time += sp.duration
    }
    return t
  }
}
