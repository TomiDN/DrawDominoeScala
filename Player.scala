package drawdominoesgame


class Player(val name: String, val pile: Deck) {
  def announcePlayer(): String = s"Now playing: ${this.name}\n"

  def printDeck(d: List[Tile] = pile.access, n: Int = 0): String =
    if (d.nonEmpty) {
      s"$n." + d.head.print + printDeck(d.tail, n+1)
    }else "\n"
}

object Player {
  def apply(name: String, pile: Deck): Player = new Player(name, pile: Deck)
}
