import scala.util.Random
import scala.util.parsing.combinator._

object Faker {

  sealed trait ItemType
  case object EscapedHash extends ItemType
  case object Hash extends ItemType
  case class Other(value: String) extends ItemType

  object ItemParser extends RegexParsers {
    def escapedHash = """\\#""".r ^^ { _ => EscapedHash }
    def hash = """#""".r ^^ { _ => Hash }
    def other = """.""".r ^^ { s => Other(s) }
    def item = escapedHash | hash | other
    def items = rep(item)
  }

  def fake(pattern: String): String = 
    ItemParser.parse(ItemParser.items, pattern).map(_.map {
      case EscapedHash => "#"
      case Hash => Random.nextInt(10).toString()
      case Other(s) => s
    }
    ).getOrElse(List(pattern)).mkString

}