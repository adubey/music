package ca.dubey.music

import javax.sound.midi.Sequence
import javax.sound.midi.MidiMessage
import javax.sound.midi.MidiEvent
import javax.sound.midi.MidiChannel
import scala.collection.mutable.ArrayBuffer

class ChannelState(val channelNumber : Int) {
  var patch : MidiPatch = MidiPatch(1)

  val isMelodic = (channelNumber != 10) || program.isMelodic
}
