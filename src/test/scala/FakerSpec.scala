import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import Faker.fake

class FakerSpec extends AnyWordSpec with Matchers {
  "Faker" when {
    "generate random number" should {
      "match one digit pattern" in {
        fake("#") should fullyMatch regex """^\d$"""
      }
      "match two digit pattern" in {
        fake("##") should fullyMatch regex """^\d\d$"""
      }
    }
    "pattern contains other characters" should {
      "return them unchanged" in {
        fake("50#-###-###") should fullyMatch regex """^50\d-\d{3}-\d{3}$"""
      }
    }
    "pattern is escaped" should {
      "return the pattern unescaped" in {
        fake("""\#""") shouldBe "#"
      }
    }
  }
}
