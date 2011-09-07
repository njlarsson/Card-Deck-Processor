import scala.collection.mutable.Stack

class Deck(name: String) {
  val cards = new Stack[Int]

  override def toString: String = {
    val b = new StringBuilder
    b ++= name
    b += '('
    var i = cards.length
    while (i > 0) {
      i = i - 1
      b ++= cards(i).toString
      if (i > 0) b ++= ", "
    }
    b += ')'
    return b.toString
  }

  private def parse(s: String) {
    val a = s.split(",")
    var i = 0
    while (i < a.length) {
      cards.push(a(i).trim.toInt)
      i = i + 1
    }
  }
  
  def read() { parse(readLine("Enter cards for " + name + ": ")) }

  def readFile(fileName: String) { parse(scala.io.Source.fromFile(fileName).mkString) }

  def moveTopTo(other: Deck) { other.cards.push(cards.pop()) }

  def moveAllTo(other: Deck) {
    other.cards.pushAll(cards)
    cards.clear
  }

  def compareTop(other: Deck): Int = {
    return cards.top - other.cards.top
  }

  def isEmpty: Boolean = cards.isEmpty
}
