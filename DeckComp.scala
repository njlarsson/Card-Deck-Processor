import java.io.FileOutputStream
import java.io.IOException
import java.io.ObjectOutputStream
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object DeckComp extends App {
  class DeckCompException(text: String) extends Exception {
    override def toString: String = lineNo + ": " + text
  }

  private var lineNo = 0

  private val decks = new HashMap[String, Int] {
    override def default(name: String) = throw new DeckCompException("No such deck: " + name)
    override def put(name: String, deck: Int): Option[Int] = {
      if (super.put(name, deck) != None) { throw new DeckCompException("Deck redefined: " + name) }
      return None
    }
  }

  private val labels = new HashMap[String, Int] {
    override def default(name: String) = throw new DeckCompException("No such label: " + name)
    override def put(name: String, lineNo: Int): Option[Int] = {
      if (super.put(name, lineNo) != None) { throw new DeckCompException("Label redefined: " + name) }
      return None
    }
  }

  try {
    var trace = false
    val fileList = new ArrayBuffer[String]
    val deckList = new ArrayBuffer[String]
    var lines: Array[String] = null
    try {
      lines = scala.io.Source.fromFile(args(0)).getLines.toArray
    } catch {
      case e => println(e)
      sys.exit(66)                      // EX_NOINPUT
    }
    val out = new ObjectOutputStream(new FileOutputStream(args(0) + ".dvm"))

    // Scan for labels, decks, and files
    val labelParser = new DeckLineParser.LineProc {
      def stop() { }
      def label(label: String) { labels(label) = lineNo + 1 }
      def makeDeck(deckName: String) {
        decks(deckName) = decks.size + 1
        deckList.append(deckName)
      }
      def moveTop(left: String, right: String) { }
      def moveAll(left: String, right: String) { }
      def jump(label: String) { }
      def jumpEmpty(deckName: String, label: String) { }
      def jumpNotEmpty(deckName: String, label: String) { }
      def jumpLess(left: String, right: String, label: String) { }
      def jumpGreater(left: String, right: String, label: String) { }
      def jumpEqual(left: String, right: String, label: String) { }
      def output(deckName: String) { }
      def read(deckName: String) {
        makeDeck(deckName)
      }
      def read(deckName: String, fileName: String) {
        makeDeck(deckName)
        fileList.append(fileName)
      }
      def parseException(message: String) = throw new DeckCompException(message)
    }
    while (lineNo < lines.length) {
      val line = lines(lineNo).trim
      lineNo = lineNo + 1
      DeckLineParser.parseLine(line, labelParser)
    }

    out.writeInt(fileList.size)
    var fileI = 0
    while (fileI < fileList.size) writeString(out, fileList(fileI))
    var deckI = 0
    while (deckI < deckList.size) writeString(out, deckList(deckI))

    val generator = new DeckLineParser.LineProc {
      def stop() { out.writeInt(DeckVM.STOP) }
      def label(label: String) { }
      def makeDeck(deckName: String) { }
      def moveTop(left: String, right: String) {
        out.writeInt(DeckVM.MOVETOP)
        out.writeInt(decks(left))
        out.writeInt(decks(right))
      }
      def moveAll(left: String, right: String) {
        out.writeInt(DeckVM.MOVEALL)
        out.writeInt(decks(left))
        out.writeInt(decks(right))
      }
      def jump(label: String) {
        out.writeInt(DeckVM.JUMP)
        out.writeInt(labels(label))
      }
      def jumpEmpty(deckName: String, label: String) { 
        out.writeInt(DeckVM.JUMP_EMPTY)
        out.writeInt(decks(deckName))
        out.writeInt(labels(label))
      }
      def jumpNotEmpty(deckName: String, label: String) {
        out.writeInt(DeckVM.JUMP_NOT_EMPTY)
        out.writeInt(decks(deckName))
        out.writeInt(labels(label))
      }
      def jumpLess(left: String, right: String, label: String) {
        out.writeInt(DeckVM.JUMP_LESS)
        out.writeInt(decks(left))
        out.writeInt(decks(right))
        out.writeInt(labels(label))
      }
      def jumpGreater(left: String, right: String, label: String) {
        out.writeInt(DeckVM.JUMP_GREATER)
        out.writeInt(decks(left))
        out.writeInt(decks(right))
        out.writeInt(labels(label))
      }
      def jumpEqual(left: String, right: String, label: String) {
        out.writeInt(DeckVM.JUMP_EQUAL)
        out.writeInt(decks(left))
        out.writeInt(decks(right))
        out.writeInt(labels(label))
      }
      def output(deckName: String) {
        out.writeInt(DeckVM.OUTPUT)
        out.writeInt(decks(deckName))
      }
      def read(deckName: String) {
        out.writeInt(DeckVM.READ)
        out.writeInt(decks(deckName))
      }
      def read(deckName: String, fileName: String) {
        out.writeInt(DeckVM.OUTPUT)
        out.writeInt(decks(deckName))
        out.writeInt(fileI)
        fileI = fileI + 1
      }
      def parseException(message: String) = throw new DeckCompException(message)
    }
    
    out.writeInt(lines.size)
    fileI = 1
    lineNo = 0
    while (lineNo < lines.length) {
      val line = lines(lineNo).trim
      lineNo = lineNo + 1
      DeckLineParser.parseLine(line, generator)
    }
  } catch {
    case e: DeckCompException => {
      println(e);
      sys.exit(65)                      // EX_DATAERR
    }
    case e: Deck.OpException => {
      println(lineNo + ": " + e.getMessage());
      sys.exit(65)                      // EX_DATAERR
    }
    case e: IOException => {
      println(e);
      sys.exit(66)                      // EX_NOINPUT
    }
  }

  private def writeString(out: ObjectOutputStream, s: String) {
    out.writeInt(s.length)
    var i = 0
    while (i < s.length) {
      out.writeChar(s(i))
      i = i + 1
    }
  }
}
