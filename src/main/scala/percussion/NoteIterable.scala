package ca.dubey.music.percussion

import ca.dubey.music.midi.event.NoteEvent
import cc.mallet.types.FeatureVector
import cc.mallet.types.Instance
import cc.mallet.types.LabelAlphabet

/**
 * Operations over a sequence of notes.
 *
 * Could be one track in a song, or a shorter motif.
 */
class NoteIterable(var notes : Iterable[NoteEvent]) {

  var ppq : Int = 24
  var n : Int = 4
  var d : Int = 4
  var beat : Int = 24
  var pulse : Int = 96

  def setPpq(ppq_ : Int) = {
    ppq = ppq_
    updateTimes
  }

  def setTimeSignature(n_ : Int, d_ : Int) = {
    n = n_
    d = d_
    updateTimes
  }

  def updateTimes = {
    beat = ppq / (d/4)
    pulse = beat * n
  }

  def extractInstances(
      alphabet : LabelAlphabet,
      targetAlphabet : LabelAlphabet,
      historySize : Int) : List[Instance] = {
    val l = List.newBuilder[Instance]
    val h = new History(historySize, alphabet, ppq)
    for (note <- notes) {
      val target = targetAlphabet.lookupLabel(Data.noteEventToString(note, note.tick, 0, ppq))
      l += new Instance(h.featureVector, target, "", "")
      h += note
    }
    return l.result
  }
}
