object DeckLineParser {
  trait LineProc {
    def stop
    def label(label: String)
    def makeDeck(deckName: String)
    def moveTop(left: String, right: String)
    def moveAll(left: String, right: String)
    def jump(label: String)
    def jumpEmpty(deckName: String, label: String)
    def jumpNotEmpty(deckName: String, label: String)
    def jumpLess(left: String, right: String, label: String)
    def jumpGreater(left: String, right: String, label: String)
    def jumpEqual(left: String, right: String, label: String)
    def output(deckName: String)
    def read(deckName: String)
    def read(deckName: String, fileName: String)
    def parseException(message: String)
  }

  private val Label =        """(\w+):""".r
  private val Instr =        """(\w+)\s+(.*)""".r
  private val OneParam =     """(\w+)""".r
  private val TwoParam =     """(\w+)\s*,\s*(\w+)""".r
  private val DeckFile =     """(\w+)\s*,\s*(\S.*)""".r
  private val IfComp =       """if\s+(\w+)\s+(\w+)\s*,\s*(\w+)\s*,\s*(\w+)""".r
  private val IfEmpty =      """if\s+empty\s+(\w+)\s*,\s*(\w+)""".r
  private val IfNotEmpty =   """if\s+not\s+empty\s+(\w+)\s*,\s*(\w+)""".r

  def parseLine(line: String, todo: LineProc) {
    line match {
      case "stop"       => todo.stop
      case Label(label) => todo.label(label)
      case Instr(instr, param) => {
        instr match {
          case "new"          => param match {
            case OneParam(deckName)   => todo.makeDeck(deckName)
            case _                    => todo.parseException("Illegal deck name: " + param)
          }
          case "movetop"      => param match {
            case TwoParam(from, to)   => todo.moveTop(from, to)
            case _                    => todo.parseException("Illegal movetop parameters: " + param)
          }
          case "moveall"      => param match {
            case TwoParam(from, to)   => todo.moveAll(from, to)
            case _                    => todo.parseException("Illegal moveall parameters: " + param)
          }
          case "jump"         => param match {
            case IfEmpty(deckName, label)             => todo.jumpEmpty(deckName, label)
            case IfNotEmpty(deckName, label)          => todo.jumpNotEmpty(deckName, label)
            case IfComp(cond, left, right, label)     => {
              cond match {
                case "less"           => todo.jumpLess(left, right, label)
                case "greater"        => todo.jumpGreater(left, right, label)
                case "equal"          => todo.jumpEqual(left, right, label)
                case _                => todo.parseException("Expected less, greater, or equal: " + cond)
              }
            }
            case OneParam(label)      => todo.jump(label)
            case _                    => todo.parseException("Illegal jump parameters: " + param)
          }
          case "output"       => param match {
            case OneParam(deckName)        => todo.output(deckName)
            case _                         => todo.parseException("Illegal deck name: " + param)
          }
          case "read"         => param match {
            case OneParam(deckName)           => todo.read(deckName)
            case DeckFile(deckName, fileName) => todo.read(deckName, fileName)
            case _                            => todo.parseException("Illegal read parameters: " + param)
          }
          case _              => todo.parseException("Illegal instruction: " + instr)
        }
      }
      case ""           => // empty line, ignore
      case _            => todo.parseException("Unrecognized syntax: " + line)
    }
  }
}
