import org.scalatest.FlatSpec
import ca.dubey.music.theory.MajorScale

class MajorScaleSpec extends FlatSpec {
  "A MajorScale" should "write out all notes in major scale" in {
    assert((new MajorScale(60)).toString == "C D E F G A B")
  }
}

