import java.io.FileInputStream
import java.io.IOException
import java.io.ObjectInputStream

object DeckVM extends App {
  class DeckVMException(text: String) extends Exception {
    override def toString: String = lineNo + ": " + text
  }

  val MOVETOP = 32 + 0
  val MOVEALL = 32 + 1
  val JUMP_EMPTY = 64 + 2
  val JUMP_NOT_EMPTY = 64 + 3
  val JUMP_LESS = 64 + 32 + 4
  val JUMP_GREATER = 64 + 32 + 5
  val JUMP_EQUAL = 64 + 32 + 6
  val JUMP = 64 + 7
  val OUTPUT = 16 + 8
  val READ = 16 + 9
  val READ_FILE = 48 + 10
  val STOP = 11

  private class Line(val op: Int, val arg1: Int, val arg2: Int, val arg3: Int) {
    override def toString: String = {
      val s = new StringBuilder(op.toString)
      val opGroup1 = (op/16) % 4
      val opGroup2 = op/64
      if (opGroup1 > 0) s.append(" " + arg1)
      if (opGroup1 > 1) s.append(" " + arg2)
      if (opGroup2 > 0) s.append(" " + arg3)
      return s.toString
    }
  }
  
  private var lineNo = 0
  private var stop = false

  try {
    var trace = false
    var decks: Array[Deck] = null
    var files: Array[String] = null
    var lines: Array[Line] = null
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
      val in = new ObjectInputStream(new FileInputStream(args(argNo)))
      files = new Array[String](in.readInt())
      var fileI = 0
      while (fileI < files.length) {
        files(fileI) = readString(in)
        fileI = fileI + 1
      }
      decks = new Array[Deck](in.readInt())
      var deckI = 0
      while (deckI < decks.length) {
        decks(deckI) = new Deck(readString(in))
        deckI = deckI + 1
      }
      lines = new Array[Line](in.readInt())
      var lineI = 0
      while (lineI < lines.length) {
        val op = in.readByte()
        var arg1 = 0
        var arg2 = 0
        var arg3 = 0
        val opGroup1 = (op/16) % 4
        val opGroup2 = op/64
        if (opGroup1 > 0) arg1 = in.readInt()
        if (opGroup1 > 1) arg2 = in.readInt()
        if (opGroup2 > 0) arg3 = in.readInt()
        lines(lineI) = new Line(op, arg1, arg2, arg3)
      }
    } catch {
      case e: IOException => {
        println(e)
        sys.exit(74)                    // EX_IOERR
      }        
    }

    while (!stop) {
      if (lineNo == lines.length) { throw new DeckVMException("Unexpected end of file") }
      val line = lines(lineNo)
      lineNo = lineNo + 1
      if (trace) { println("Executing " + lineNo + ": " + line) }
      else if (line.op == MOVETOP) decks(line.arg1 - 1).moveTopTo(decks(line.arg2 - 1))
      else if (line.op == MOVEALL) decks(line.arg1 - 1).moveAllTo(decks(line.arg2 - 1))
      else if (line.op == JUMP_EMPTY) { if (decks(line.arg1 - 1).isEmpty) lineNo = line.arg3 - 1 }
      else if (line.op == JUMP_NOT_EMPTY) { if (!decks(line.arg1 - 1).isEmpty) lineNo = line.arg3 - 1 }
      else if (line.op == JUMP_LESS) { if (decks(line.arg1 - 1).compareTop(decks(line.arg2 - 1)) < 0) lineNo = line.arg3 - 1 }
      else if (line.op == JUMP_GREATER) { if (decks(line.arg1 - 1).compareTop(decks(line.arg2 - 1)) > 0) lineNo = line.arg3 - 1 }
      else if (line.op == JUMP_EQUAL) { if (decks(line.arg1 - 1).compareTop(decks(line.arg2 - 1)) == 0) lineNo = line.arg3 - 1 }
      else if (line.op == JUMP) lineNo = line.arg3 - 1
      else if (line.op == OUTPUT) println(decks(line.arg1 - 1))
      else if (line.op == READ) decks(line.arg1 - 1).read()
      else if (line.op == READ_FILE) decks(line.arg1 - 1).readFile(files(line.arg2 - 1))
      else stop = true
    }
  } catch {
    case e: DeckVMException => {
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

  private def readString(in: ObjectInputStream): String = {
    val length = in.readInt()
    val s = new StringBuilder()
    var i = 0
    while (i < length) {
      s.append(in.readChar())
      i = i + 1
    }
    return s.toString
  }
}
