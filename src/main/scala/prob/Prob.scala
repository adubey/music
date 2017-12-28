package ca.dubey.music.prob

/**
  * Table probabilities
  * V2
  */
class Prob(val distribution : Array[Double]) {
  val r = new util.Random
  def sample : Int = {
    var p = r.nextDouble
    var i = 0
    do {
      p -= distribution(i)
      if (p <= 0.0) {
        return i
      }
      i += 1
    } while (i < distribution.size)
    return i-1
  }
}
