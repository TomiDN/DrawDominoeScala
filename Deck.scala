package drawdominoesgame

import scala.collection.mutable.ArrayBuffer


class Deck(private val buffer: ArrayBuffer[Tile]) {
  def isEmpty: Boolean = buffer.isEmpty

  def length: Int = buffer.length

  def last: Tile = buffer.last

  def access: ArrayBuffer[Tile] = this.buffer

  def add(elem: Tile): ArrayBuffer[Tile] = buffer += elem

  def use(elem: Tile): Tile = {
    buffer -= elem
    elem
  }

  def moveToEnd(elem: Tile): Tile = {
    this.add(this.use(elem))
    buffer.last
  }

  def indexOf(elem: Tile): Int = buffer.indexOf(elem)

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
  def apply(b: ArrayBuffer[Tile]): Deck = new Deck(b)
}

