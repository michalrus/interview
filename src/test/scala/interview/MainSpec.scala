package interview

import org.scalatest.WordSpec

class MainSpec extends UnitSpec {
  "Greeting" should {
    "be nice" in {
      Main.Greeting shouldBe "Hello, World!"
    }
  }

  "Addition" should {
    "be commutative" in forAll { (a: Int, b: Int) ⇒ b + a shouldEqual a + b }
  }
}
