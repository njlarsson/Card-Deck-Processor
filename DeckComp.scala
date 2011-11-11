import java.io.FileOutputStream
import java.io.IOException
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
    val out = new FileOutputStream(args(0) + ".dmach")
    var instrNo = 1

    // Scan for labels, decks, and files
    val pass1 = new DeckLineParser.LineProc {
      def stop() { instrNo = instrNo + 1 }
      def label(label: String) { labels(label) = instrNo }
      def makeDeck(deckName: String) {
        decks(deckName) = decks.size + 1
        deckList.append(deckName)
      }
      def moveTop(left: String, right: String) { instrNo = instrNo + 1 }
      def moveAll(left: String, right: String) { instrNo = instrNo + 1 }
      def jump(label: String) { instrNo = instrNo + 1 }
      def jumpEmpty(deckName: String, label: String) { instrNo = instrNo + 1 }
      def jumpNotEmpty(deckName: String, label: String) { instrNo = instrNo + 1 }
      def jumpLess(left: String, right: String, label: String) { instrNo = instrNo + 1 }
      def jumpGreater(left: String, right: String, label: String) { instrNo = instrNo + 1 }
      def jumpEqual(left: String, right: String, label: String) { instrNo = instrNo + 1 }
      def output(deckName: String) { instrNo = instrNo + 1 }
      def read(deckName: String) {
        makeDeck(deckName)
        instrNo = instrNo + 1
      }
      def read(deckName: String, fileName: String) {
        makeDeck(deckName)
        fileList.append(fileName)
        instrNo = instrNo + 1
      }
      def parseException(message: String) = throw new DeckCompException(message)
    }
    while (lineNo < lines.length) {
      val line = lines(lineNo).trim
      lineNo = lineNo + 1
      DeckLineParser.parseLine(line, pass1)
    }

    if (fileList.size > 255) throw new DeckCompException("Too many files: " + fileList.size)
    out.write(fileList.size)
    var fileI = 0
    while (fileI < fileList.size) {
      writeString(out, fileList(fileI))
      fileI = fileI + 1
    }
    if (deckList.size > 255) throw new DeckCompException("Too many decks: " + deckList.size)
    out.write(deckList.size)
    var deckI = 0
    while (deckI < deckList.size) {
      writeString(out, deckList(deckI))
      deckI = deckI + 1
    }

    val pass2 = new DeckLineParser.LineProc {
      def stop() { out.write(DeckVM.STOP) }
      def label(label: String) { }
      def makeDeck(deckName: String) { }
      def moveTop(left: String, right: String) {
        out.write(DeckVM.MOVETOP)
        out.write(decks(left))
        out.write(decks(right))
      }
      def moveAll(left: String, right: String) {
        out.write(DeckVM.MOVEALL)
        out.write(decks(left))
        out.write(decks(right))
      }
      def jump(label: String) {
        out.write(DeckVM.JUMP)
        out.write(labels(label))
      }
      def jumpEmpty(deckName: String, label: String) { 
        out.write(DeckVM.JUMP_EMPTY)
        out.write(decks(deckName))
        out.write(labels(label))
      }
      def jumpNotEmpty(deckName: String, label: String) {
        out.write(DeckVM.JUMP_NOT_EMPTY)
        out.write(decks(deckName))
        out.write(labels(label))
      }
      def jumpLess(left: String, right: String, label: String) {
        out.write(DeckVM.JUMP_LESS)
        out.write(decks(left))
        out.write(decks(right))
        out.write(labels(label))
      }
      def jumpGreater(left: String, right: String, label: String) {
        out.write(DeckVM.JUMP_LESS)
        out.write(decks(right))
        out.write(decks(left))
        out.write(labels(label))
      }
      def jumpEqual(left: String, right: String, label: String) {
        out.write(DeckVM.JUMP_EQUAL)
        out.write(decks(left))
        out.write(decks(right))
        out.write(labels(label))
      }
      def output(deckName: String) {
        out.write(DeckVM.OUTPUT)
        out.write(decks(deckName))
      }
      def read(deckName: String) {
        out.write(DeckVM.READ)
        out.write(decks(deckName))
      }
      def read(deckName: String, fileName: String) {
        out.write(DeckVM.READ_FILE)
        out.write(decks(deckName))
        out.write(fileI)
        fileI = fileI + 1
      }
      def parseException(message: String) = throw new DeckCompException(message)
    }
    
    if (instrNo > 255) throw new DeckCompException("Too many instructions: " + instrNo)
    out.write(instrNo - 1)
    fileI = 1
    lineNo = 0
    while (lineNo < lines.length) {
      val line = lines(lineNo).trim
      lineNo = lineNo + 1
      DeckLineParser.parseLine(line, pass2)
    }
    out.close()
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

  private def writeString(out: FileOutputStream, s: String) {
    if (s.length > 255) throw new DeckCompException("String too long: " + s)
    out.write(s.length)
    var i = 0
    while (i < s.length) {
      if (s(i) > 255) throw new DeckCompException("Illegal character: " + s(i))
      out.write(s(i))
      i = i + 1
    }
  }
}
