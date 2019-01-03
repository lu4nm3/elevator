package elevator

import org.scalatest.WordSpec

class TestSpec extends WordSpec {
  "Test" should {
    "times by 2" in {
      val result = new Test(3).timeTwo

      assertResult(6)(result)
    }
  }

}
