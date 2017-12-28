package ca.dubey.music.midi

import javax.sound.midi.Sequence
import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiChannel
import scala.collection.mutable.ArrayBuffer

case class ChannelInfo(val channelNumber : Int) {
  var patch : Patch = Patch(1)

  /**
    * Return true if this is a melodic (non-percussive or sound effects) track 
    */
  val isMelodic = (channelNumber != 10) || patch.isMelodic
}
