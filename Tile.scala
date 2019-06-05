package drawdominoes

class Tile(val a: Int, val b: Int) {

  def ==(t: Tile): Boolean = this.a == t.a && this.b == t.b

  def !=(t: Tile): Boolean = !(this == t)

  def print: String = s"$a|$b "

}

object Tile {

  def apply(x: Int, y: Int): Tile = new Tile(x, y)

}
