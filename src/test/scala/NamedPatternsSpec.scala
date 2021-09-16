import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import Faker.fake
import org.scalatest.TryValues
import scala.util.Try

class NamedPatternsSpec extends AnyWordSpec with Matchers with TryValues {
  "Faker" when {
    "A named pattern is used" should {
      "use a built-in function" in {
        fake("#{randomPassword}") should fullyMatch regex """^.{8}$"""
      }
      "return an unknown pattern unchanged" in {
        fake("#{unknownPattern}") shouldBe "#{unknownPattern}"
      }
    }
  }
}
