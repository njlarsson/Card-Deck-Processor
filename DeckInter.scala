import scala.collection.mutable.HashMap

object DeckInter extends App {
  class DeckInterException(text: String) extends Exception {
    override def toString: String = lineNo + ": " + text
  }

  private var lineNo = 0
  private var stop = false

  private val decks = new HashMap[String, Deck] {
    override def default(name: String) = throw new DeckInterException("No such deck: " + name)
    override def put(name: String, deck: Deck): Option[Deck] = {
      if (super.put(name, deck) != None) { throw new DeckInterException("Deck redefined: " + name) }
      return None
    }
  }

  private val labels = new HashMap[String, Int] {
    override def default(name: String) = throw new DeckInterException("No such label: " + name)
    override def put(name: String, lineNo: Int): Option[Int] = {
      if (super.put(name, lineNo) != None) { throw new DeckInterException("Label redefined: " + name) }
      return None
    }
  }

  try {
    var trace = false
    var lines: Array[String] = null
    var argNo = 0
    while (argNo < args.length - 1) {
      args(argNo) match {
        case "-trace" => trace = true
        case u => {
          println("Unrecognized option: " + u)
          sys.exit(64)                  // EX_USAGE
        }
      }
      argNo = argNo + 1
    }
    if (argNo == args.length) {
      println("Missing file name")
      sys.exit(64)
    }
    try {
      lines = scala.io.Source.fromFile(args(argNo)).getLines.toArray
    } catch {
      case e => println(e)
      sys.exit(66)                      // EX_NOINPUT
    }

    // Scan for labels.
    val labelParser = new DeckLineParser.LineProc {
      def stop() { }
      def label(label: String) { labels(label) = lineNo }
      def makeDeck(deckName: String) { }
      def moveTop(left: String, right: String) { }
      def moveAll(left: String, right: String) { }
      def jump(label: String) { }
      def jumpEmpty(deckName: String, label: String) { }
      def jumpNotEmpty(deckName: String, label: String) { }
      def jumpLess(left: String, right: String, label: String) { }
      def jumpGreater(left: String, right: String, label: String) { }
      def jumpEqual(left: String, right: String, label: String) { }
      def output(deckName: String) { }
      def read(deckName: String) { }
      def read(deckName: String, fileName: String) { }
      def parseException(message: String) = throw new DeckInterException(message)
    }
    while (lineNo < lines.length) {
      val line = lines(lineNo).trim
      lineNo = lineNo + 1
      DeckLineParser.parseLine(line, labelParser)
    }
    lineNo = 0                            // reset for execution

    val executor = new DeckLineParser.LineProc {
      def stop() { DeckInter.stop = true }
      def label(label: String) { }
      def makeDeck(deckName: String) { DeckInter.makeDeck(deckName) }
      def moveTop(left: String, right: String) { decks(left).moveTopTo(decks(right)) }
      def moveAll(left: String, right: String) { decks(left).moveAllTo(decks(right)) }
      def jump(label: String) { lineNo = labels(label) }
      def jumpEmpty(deckName: String, label: String) { 
        if (decks(deckName).isEmpty) { lineNo = labels(label) } 
      }
      def jumpNotEmpty(deckName: String, label: String) {
        if (!decks(deckName).isEmpty) { lineNo = labels(label) }
      }
      def jumpLess(left: String, right: String, label: String) {
        if (decks(left).compareTop(decks(right)) < 0) { lineNo = labels(label) }
      }
      def jumpGreater(left: String, right: String, label: String) {
        if (decks(left).compareTop(decks(right)) > 0) { lineNo = labels(label) }
      }
      def jumpEqual(left: String, right: String, label: String) {
        if (decks(left).compareTop(decks(right)) == 0) { lineNo = labels(label) }
      }
      def output(deckName: String) = println(decks(deckName))
      def read(deckName: String) = DeckInter.makeDeck(deckName).read()
      def read(deckName: String, fileName: String) = DeckInter.makeDeck(deckName).readFile(fileName)
      def parseException(message: String) = throw new DeckInterException(message)
    }

    while (!stop) {
      if (lineNo == lines.length) { throw new DeckInterException("Unexpected end of file") }
      val line = lines(lineNo).trim
      lineNo = lineNo + 1
      if (trace) { println("Executing " + lineNo + ": " + line) }
      DeckLineParser.parseLine(line, executor)
    }
  } catch {
    case e: DeckInterException => {
      println(e);
      sys.exit(65)                      // EX_DATAERR
    }
    case e: Deck.OpException => {
      println(lineNo + ": " + e.getMessage());
      sys.exit(65)                      // EX_DATAERR
    }
    case e: java.io.IOException => {
      println(e);
      sys.exit(66)                      // EX_NOINPUT
    }
  }

  private def makeDeck(deckName: String): Deck = {
    val deck = new Deck(deckName)
    decks(deckName) = deck
    return deck
  }
}
