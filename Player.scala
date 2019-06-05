package drawdominoes

class Player(val name: String, val pile: Deck[Tile]) {

  def getTileFromBN(boneyard: Deck[Tile], position: Int): Deck[Tile] = boneyard.getAt(position) match {

    case Invalid(_) => pile

    case Valid(t) => pile.add(t)

  }

  def drawTileFromBN(boneyard: Deck[Tile]): Validated[String, Deck[Tile]] = boneyard.length match {

    case 0 => Invalid("Boneyard is empty!")

    case _ => Valid(getTileFromBN(boneyard, boneyard.randomPosition))

  }

  def drawTileFromPile(index: Int): Validated[String, Tile] =  pile.getAt(index) match {

    case i @ Invalid(_) => i

    case p @ Valid(t) =>

      pile.use(t)

      p

  }

  def announcePlayer(): String = s"Now playing: ${this.name}\n"

  def printDeck(d: List[Tile] = pile.unused): String =

    if (d.nonEmpty) {

      pile.head.print + printDeck(d.tail)

    }else "\n"

}

object Player {

  def fillDeck(deck: Deck[Tile], boneyard: Deck[Tile]): Deck[Tile] =

    deck.length match {

      case 7 => deck

      case _ => boneyard.getAt(boneyard.randomPosition) match {

        case Invalid(_) => fillDeck(deck, boneyard)

        case Valid(t: Tile) =>

          deck.use(t)

          fillDeck(deck.add(t), boneyard)

      }

    }

  def apply(name: String, boneyard: Deck[Tile]): Player = new Player(name, fillDeck(new Deck[Tile](Nil, Nil), boneyard))

}
