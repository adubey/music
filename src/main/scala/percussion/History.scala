package ca.dubey.music.percussion

import ca.dubey.music.midi.event.NoteEvent
import ca.dubey.music.midi.event.Skip
import cc.mallet.types.LabelAlphabet
import cc.mallet.types.FeatureVector

class History(val size : Int, val alphabet : LabelAlphabet, val ppq : Int) {
  val contents = Array.fill[NoteEvent](size)(Skip(0))

  if (size == 0) {
    throw new Exception("Size is 0")
  }

  override def clone : History = {
    val other = new History(size, alphabet, ppq)
    for (i <- 0 until size) {
      other.contents(i) = contents(i)
    }
    return other
  }

  def +=(data : NoteEvent) = {
    for (i <- 1 until size) {
      contents(i) = contents(i-1)
    }
    contents(0) = data
  }

  def featureVector : FeatureVector = {
    val d = Array.fill(size)(0)
    var sumTime : Long = -contents(0).tick
    for (i <- 0 until size) {
      d(i) = alphabet.lookupLabel(Data.noteEventToString(contents(i), sumTime, i, ppq)).getIndex
      sumTime += contents(i).tick
    }
    return new FeatureVector(alphabet, d)
  }
}
