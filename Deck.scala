package drawdominoesgame


class Deck(private val buffer: List[Tile]) {
  def isEmpty: Boolean = buffer.isEmpty

  def length: Int = buffer.length

  def last: Tile = buffer.last

  def head: Tile = buffer.head

  def access: List[Tile] = this.buffer

  def diff(other: Deck): Deck = Deck(this.buffer.diff(other.access))

  def add(elem: Tile): Deck = Deck(buffer :+ elem)

  def remove(value: Tile, buf: List[Tile] = this.buffer): List[Tile] = buf match {
    case `value` :: tail =>  tail
    case x :: tail => x :: remove(value, tail)
    case _ => Nil
  }

  def use(elem: Tile): Deck = Deck(remove(elem))

  def moveToEnd(elem: Tile): Deck = this.use(elem).add(elem)

  def indexOf(elem: Tile): Int = buffer.indexOf(elem)

  def getRandomElement: Tile = buffer(randomPosition)

  def getAt(index: Int): Validated[String, Tile] =
    if (index >= this.length || index < 0 || this.isEmpty) {
      Invalid(s"Invalid index!")
    } else Valid(buffer(index))

  def randomPosition: Int = {
    val start = 0
    val end = length - 1
    val rnd = new scala.util.Random
    start + rnd.nextInt((end - start) + 1)
  }
}

object Deck {
  def apply(b: List[Tile]): Deck = new Deck(b)
}

