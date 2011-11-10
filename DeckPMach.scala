import java.io.FileInputStream
import java.io.IOException

object DeckPMach extends App {
  var instrI = 0
  try {
    val in = new FileInputStream(args(0))
    val nFiles = in.read()
    var fileI = 0
    println("Files:")
    while (fileI < nFiles) {
      fileI = fileI + 1
      val s = DeckVM.readString(in)
      println(DeckVM.pad(fileI + ":", 6) + encoding(s) + "(" + s + ")")
    }
    val nDecks = in.read()
    var deckI = 0
    println("Decks:")
    while (deckI < nDecks) {
      deckI = deckI + 1
      val s = DeckVM.readString(in)
      println(DeckVM.pad(deckI + ":", 6) + encoding(s) + "(" + s + ")")
    }
    val nInstrs = in.read()
    println("Instructions:")
    while (instrI < nInstrs) {
      instrI = instrI + 1
      val op = in.read()
      var arg1 = 0
      var arg2 = 0
      var arg3 = 0
      val opGroup1 = (op/16) % 4
      val opGroup2 = op/64
      if (opGroup1 > 0) arg1 = in.read()
      if (opGroup1 > 1) arg2 = in.read()
      if (opGroup2 > 0) arg3 = in.read()
      println(DeckVM.pad(instrI + ":", 6) + new DeckVM.Instr(op, arg1, arg2, arg3))
    }
    if (in.read() != -1) println("Garbage at end of file")
  } catch {
    case e: IOException => {
      println(e)
      sys.exit(74)                    // EX_IOERR
    }        
    case e: Deck.OpException => {
      println(instrI + ": " + e.getMessage());
      sys.exit(65)                      // EX_DATAERR
    }
  }

  def encoding(orig: String): String = {
    val s = new StringBuilder(orig.length + " ")
    var i = 0
    while (i < orig.length) {
      s.append(orig(i).toInt + " ")
      i = i + 1
    }
    return s.toString
  }
}
