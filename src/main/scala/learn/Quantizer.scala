package ca.dubey.music.learn

import scala.collection.immutable.TreeSet

object Quantizer {
  def apply(xs : Int*) : Quantizer = {
    val set = (TreeSet.newBuilder[Int] ++= xs).result
    return new Quantizer(set, set.map((x : Int) => -x))
  }
}

class Quantizer(val set : TreeSet[Int], negativeSet : TreeSet[Int]) extends Iterable[Int] {
  override def iterator : Iterator[Int] = set.iterator

  def quantize(x : Int) : Int = {
    val i1 = set.iteratorFrom(x)
    val i2 = set.iteratorFrom(-x)

    // assert(i1.hasNext || i2.hasNext)

    if (!i1.hasNext) {
      val i = i2.next
      return i
    }

    if (!i2.hasNext) {
      val i = i1.next
      return i
    }

    val a = i1.next
    val b = i2.next

    if (math.abs(a-x) <= math.abs(b-x)) {
      a
    } else {
      b
    }
  }
}
