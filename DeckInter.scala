import scala.collection.mutable.HashMap

object DeckInter extends App {
  class BadSyntax(lineNo: Int, text: String) extends Exception {
    override def toString = "Bad syntax on line " + lineNo + ": " + text
  }

  val lines = scala.io.Source.fromFile(args(0)).getLines.toArray
  val decks = new HashMap[String, Deck]
  val labels = new HashMap[String, Int]
  var lineNo = 0
  val Label =        """(\w+):""".r
  val Decl =         """new\s+deck\s+(\w+)""".r
  val MoveTop =      """move\s+top\s+(\w+)\s*,\s*(\w+)""".r
  val MoveAll =      """move\s+all\s+(\w+)\s*,\s*(\w+)""".r
  val Jump =         """jump\s+(\w+)""".r
  val JumpComp =     """jump\s+if\s+(\w)\s+(\w+)\s*,\s*(\w+)\s*,\s*(\w+)""".r
  val JumpEmpty =    """jump\s+if\s+empty\s+(\w+)\s*,\s*(\w+)""".r
  val JumpNotEmpty = """jump\s+if\s+not\s+empty\s+(\w+)\s*,\s*(\w+)""".r
  val Stop =         """stop""".r
  var stop = false
  while (!stop) {
    val line = if (lineNo == lines.length) "stop" else lines(lineNo).trim
    line match {
      case Label(label)                  => labels.put(label, lineNo+1)
      case Decl(name)                    => decks.put(name, new Deck(name))
      case MoveTop(from, to)             => decks(from).moveTopTo(decks(to))
      case MoveAll(from, to)             => decks(from).moveAllTo(decks(to))
      case Jump(to)                      => lineNo = labels(to)
      case JumpComp(cond, lt, rt, label) => {
        val cmp = decks(lt).compareTop(decks(rt))
        cond match {
          case "less"    => if (cmp < 0) lineNo = labels(label)
          case "greater" => if (cmp > 0) lineNo = labels(label)
          case "equal"   => if (cmp == 0) lineNo = labels(label)
          case _         => throw new BadSyntax(lineNo, "Expected less, greater, or equal: " + cond)
        }
      }
      case JumpEmpty(deck, label)        => if (decks(deck).isEmpty) lineNo = labels(label)
      case JumpNotEmpty(deck, label)     => if (!decks(deck).isEmpty) lineNo = labels(label)
      case Stop                          => stop = true
      case _                             => throw new BadSyntax(lineNo, line)
    }
  }
}
