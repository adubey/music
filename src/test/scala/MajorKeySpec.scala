import org.scalatest.FlatSpec
import ca.dubey.music.MajorKey

class MajorKeySpec extends FlatSpec {
  "A MajorKey" should "write out all notes in scale" in {
    assert((new MajorKey(60)).toString == "C D E F G A B")
  }
}

