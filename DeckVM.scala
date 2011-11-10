import java.io.FileInputStream
import java.io.IOException

object DeckVM extends App {
  class DeckVMException(text: String) extends Exception {
    override def toString: String = instrNo + ": " + text
  }

  def MOVETOP = 32 + 1
  def MOVEALL = 32 + 2
  def JUMP_EMPTY = 64 + 16 + 3
  def JUMP_NOT_EMPTY = 64 + 16 + 4
  def JUMP_LESS = 64 + 32 + 5
  def JUMP_EQUAL = 64 + 32 + 6
  def JUMP = 64 + 7
  def OUTPUT = 16 + 8
  def READ = 16 + 9
  def READ_FILE = 48 + 10
  def STOP = 11

  class Instr(val op: Int, val arg1: Int, val arg2: Int, val arg3: Int) {
    override def toString: String = {
      val s = new StringBuilder(pad(op.toString, 5))
      val opGroup1 = (op/16) % 4
      val opGroup2 = op/64
      if (opGroup1 > 0) s.append(pad(arg1.toString, 4))
      if (opGroup1 > 1) s.append(pad(arg2.toString, 4))
      if (opGroup2 > 0) s.append(pad(arg3.toString, 4))
      return s.toString
    }
  }

  def pad(orig: String, l: Int): String = {
    val s = new StringBuilder(orig)
    var p = l - s.length
    while (p > 0) {
      s.append(" ")
      p = p - 1
    }
    return s.toString
  }
  
  private var instrNo = 0
  private var stop = false

  try {
    var trace = false
    var decks: Array[Deck] = null
    var files: Array[String] = null
    var instrs: Array[Instr] = null
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
    var instrI = 0
    try {
      val in = new FileInputStream(args(argNo))
      files = new Array[String](in.read())
      var fileI = 0
      while (fileI < files.length) {
        files(fileI) = readString(in)
        fileI = fileI + 1
      }
      decks = new Array[Deck](in.read())
      var deckI = 0
      while (deckI < decks.length) {
        decks(deckI) = new Deck(readString(in))
        deckI = deckI + 1
      }
      instrs = new Array[Instr](in.read())
      while (instrI < instrs.length) {
        instrNo = instrI + 1
        val op = in.read()
        var arg1 = 0
        var arg2 = 0
        var arg3 = 0
        val opGroup1 = (op/16) % 4
        val opGroup2 = op/64
        if (opGroup1 > 0) arg1 = in.read()
        if (opGroup1 > 1) arg2 = in.read()
        if (opGroup2 > 0) arg3 = in.read()
        instrs(instrI) = new Instr(op, arg1, arg2, arg3)
        instrI = instrI + 1
      }
    } catch {
      case e: IOException => {
        println(e)
        sys.exit(74)                    // EX_IOERR
      }        
    }

    instrI = 0
    while (!stop) {
      instrNo = instrI + 1
      if (instrI == instrs.length) { throw new DeckVMException("Unexpected end of file") }
      val instr = instrs(instrI)
      instrI = instrI + 1
      if (trace) { println("Executing " + instrNo + ": " + instr) }
      if (instr.op == MOVETOP) decks(instr.arg1 - 1).moveTopTo(decks(instr.arg2 - 1))
      else if (instr.op == MOVEALL) decks(instr.arg1 - 1).moveAllTo(decks(instr.arg2 - 1))
      else if (instr.op == JUMP_EMPTY) { if (decks(instr.arg1 - 1).isEmpty) instrI = instr.arg3 - 1 }
      else if (instr.op == JUMP_NOT_EMPTY) { if (!decks(instr.arg1 - 1).isEmpty) instrI = instr.arg3 - 1 }
      else if (instr.op == JUMP_LESS) { if (decks(instr.arg1 - 1).compareTop(decks(instr.arg2 - 1)) < 0) instrI = instr.arg3 - 1 }
      else if (instr.op == JUMP_EQUAL) { if (decks(instr.arg1 - 1).compareTop(decks(instr.arg2 - 1)) == 0) instrI = instr.arg3 - 1 }
      else if (instr.op == JUMP) instrI = instr.arg3 - 1
      else if (instr.op == OUTPUT) println(decks(instr.arg1 - 1))
      else if (instr.op == READ) decks(instr.arg1 - 1).read()
      else if (instr.op == READ_FILE) decks(instr.arg1 - 1).readFile(files(instr.arg2 - 1))
      else stop = true
    }
  } catch {
    case e: DeckVMException => {
      println(e);
      sys.exit(65)                      // EX_DATAERR
    }
    case e: Deck.OpException => {
      println(instrNo + ": " + e.getMessage());
      sys.exit(65)                      // EX_DATAERR
    }
    case e: java.io.IOException => {
      println(e);
      sys.exit(66)                      // EX_NOINPUT
    }
  }

  def readString(in: FileInputStream): String = {
    val length = in.read()
    val s = new StringBuilder()
    var i = 0
    while (i < length) {
      val c = in.read().toChar
      s.append(c)
      i = i + 1
    }
    return s.toString
  }
}
