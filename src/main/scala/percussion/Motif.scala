package ca.dubey.music.percussion

import collection.mutable.ListBuffer
import ca.dubey.music.midi.event.NoteEvent
import ca.dubey.music.midi.event.NoteOn

trait MotifSampler {
  def sampleMotif(numMeasures : Int, prev : Option[BaseMotif] = None)  : BaseMotif
}


object Motif {
  def build(sampler : MotifSampler) : Motif = {
    // Warm it up.
    val start = sampler.sampleMotif(20, None)
    // This is the actual begining.
    val a1 = sampler.sampleMotif(1, Some(start))
    val bases = Array(
      extendMotif(a1, 1, sampler),
      extendMotif(a1, 1, sampler),
      extendMotif(a1, 1, sampler),
      extendMotif(a1, 1, sampler))
    var r = new util.Random

    val motifs = ComposedMotif(ListBuffer.empty[BaseMotif])

    for (i <- 1 to 20) {
      if (i % 2 == 0) {
        val j = r.nextInt(bases.size)

        motifs += bases(j).clone
      } else {
        motifs += sampler.sampleMotif(2, motifs.last)
      }
    }

    return motifs
  }

  def extendMotif(motif : Motif, numMeasures : Int, sampler : MotifSampler) : ComposedMotif = {
    motif match {
      case b:BaseMotif =>
        val next = sampler.sampleMotif(numMeasures, Some(b))
        val composed = ComposedMotif(ListBuffer.empty[BaseMotif])
        composed += b.clone
        composed += next
        return composed
      case c:ComposedMotif =>
        c += sampler.sampleMotif(numMeasures, c.last)
        return c
    }
  }
}

abstract class Motif {
  var offset : Long
  def end : Long
  def realize(realizer : (NoteEvent)=>Unit, additionalOffset : Long = 0) : Unit
}

case class BaseMotif(val events : Array[NoteEvent], var offset : Long = 0) extends Motif {
  override def end = events(events.size-1).tick + offset

  override def clone : BaseMotif = BaseMotif(events, offset)

  override def realize(realizer : (NoteEvent)=>Unit, additionalOffset : Long = 0) : Unit = {
    for (note <- events) {
      note match {
        case on:NoteOn =>
          realizer(NoteOn(note.tick + offset + additionalOffset, note.key, on.velocity))
          printf("Realizing %d+%s\n", offset+additionalOffset, note)
      }
    }
  }

  def analyzer : Analyzer.Song = {
    val s = Analyzer.Song(events)
    s.setPpq(960)
    s.setTimeSignature(4,4)
    return s
  }
}

case class ComposedMotif(
    var motifs : ListBuffer[BaseMotif],
    var offset : Long = 0) extends Motif {
  override def end = if(motifs.isEmpty) { offset } else { motifs(motifs.size-1).end }

  override def clone : ComposedMotif = {
    ComposedMotif((motifs.map((m:BaseMotif) => m.clone)), offset)
  }

  override def realize(realizer : (NoteEvent)=>Unit, additionalOffset : Long = 0) : Unit = {
    for (motif <- motifs) {
      // This's offset should already be set via +=
      motif.realize(realizer, additionalOffset)
    }
  }

  def last : Option[BaseMotif] = motifs.isEmpty match {
    case true => None
    case false => Some(motifs(motifs.size-1))
  }

  def +=(add : Motif) : ComposedMotif = {
    add match {
      case b:BaseMotif =>
        add.offset += end
        motifs += b
      case c:ComposedMotif =>
        val cloned = c.motifs.map((m:BaseMotif) => m.clone)
        for (motif <- cloned){
          motif.offset += end
        }
        motifs ++= cloned
    }
    return this
  }
}
