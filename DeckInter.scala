import scala.collection.mutable.HashMap

object DeckInter extends App {
  class DeckException(text: String) extends Exception {
    override def toString = lineNo + ": " + text
  }

  val lines = scala.io.Source.fromFile(args(0)).getLines.toArray

  val decks = new HashMap[String, Deck] {
    override def default(name: String) = throw new DeckException("No such deck: " + name)
    override def put(name: String, deck: Deck): Option[Deck] = {
      if (super.put(name, deck) != None) throw new DeckException("Deck redefined: " + name)
      return None
    }
  }

  val labels = new HashMap[String, Int] {
    override def default(name: String) = throw new DeckException("No such label: " + name)
    override def put(name: String, lineNo: Int): Option[Int] = {
      if (super.put(name, lineNo) != None) throw new DeckException("Label redefined: " + name)
      return None
    }
  }

  var lineNo = 0

  val Label =        """(\w+):""".r
  val Decl =         """new\s+deck\s+(\w+)""".r
  val MoveTop =      """move\s+top\s+(\w+)\s*,\s*(\w+)""".r
  val MoveAll =      """move\s+all\s+(\w+)\s*,\s*(\w+)""".r
  val AnyJump =      """jump.*""".r
  val Jump =         """jump\s+(\w+)""".r
  val JumpComp =     """jump\s+if\s+(\w)\s+(\w+)\s*,\s*(\w+)\s*,\s*(\w+)""".r
  val JumpEmpty =    """jump\s+if\s+empty\s+(\w+)\s*,\s*(\w+)""".r
  val JumpNotEmpty = """jump\s+if\s+not\s+empty\s+(\w+)\s*,\s*(\w+)""".r
  val Stop =         """stop""".r
  val Print =        """print\s+(\w+)""".r
  val Read =         """read\s+(\w+)""".r
  val ReadFile =     """read\s+(\w+)\s*,\s*(\S.*)""".r
  var stop = false
  while (!stop) {
    lineNo = lineNo + 1
    val line = if (lineNo > lines.length) "stop" else lines(lineNo - 1).trim
    println("Executing " + lineNo + ": " + line);
    line match {
      case Label(label)                    => labels(label) = lineNo
      case Decl(deckName)                  => makeDeck(deckName)
      case MoveTop(from, to)             => decks(from).moveTopTo(decks(to))
      case MoveAll(from, to)             => decks(from).moveAllTo(decks(to))
      case AnyJump => line match {
        // This nesting should not be necessary, but the match must be simplified not to break compiler.

        case Jump(to)                      => lineNo = labels(to)
        case JumpComp(cond, left, right, label) => {
          val cmp = decks(left).compareTop(decks(right))
          cond match {
            case "less"    => if (cmp < 0) lineNo = labels(label)
            case "greater" => if (cmp > 0) lineNo = labels(label)
            case "equal"   => if (cmp == 0) lineNo = labels(label)
            case _         => throw new DeckException("Expected less, greater, or equal: " + cond)
          }
        }
        case JumpEmpty(deckName, label)    => if (decks(deckName).isEmpty) lineNo = labels(label)
        case JumpNotEmpty(deckName, label) => if (!decks(deckName).isEmpty) lineNo = labels(label)
      }
      case Stop()                          => stop = true
      case Print(deckName)                 => println(decks(deckName))
      case Read(deckName)                  => makeDeck(deckName).read
      case ReadFile(deckName, fileName)    => makeDeck(deckName).readFile(fileName)
      case _                               => throw new DeckException("Bad syntax: " + line)
    }
  }

  private def makeDeck(name: String): Deck = {
    val deck = new Deck(name)
    decks.put(name, deck)
    return deck
  }
}
