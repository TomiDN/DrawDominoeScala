package drawdominoesgame

import scala.collection.mutable.ArrayBuffer


class Player(val name: String, val pile: Deck) {

  def getTileFromBN(boneyard: Deck, position: Int): ArrayBuffer[Tile] = boneyard.getAt(position) match {

    case Invalid(_) => pile.access

    case Valid(t) => pile.add(boneyard.use(t))

  }


  def drawTileFromBN(boneyard: Deck): Validated[String, ArrayBuffer[Tile]] = boneyard.length match {

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


  def printDeck(d: ArrayBuffer[Tile] = pile.access, n: Int = 0): String =

    if (d.nonEmpty) {

      s"$n." + d.head.print + printDeck(d.tail, n+1)

    }else "\n"

}

object Player {

  def fillDeck(deck: ArrayBuffer[Tile], boneyard: Deck): Deck =

    deck.length match {

      case 7 => new Deck(deck)

      case _ => boneyard.getAt(boneyard.randomPosition) match {

        case Invalid(_) => fillDeck(deck, boneyard)

        case Valid(t: Tile) =>

          boneyard.use(t)

          fillDeck(deck += t, boneyard)

      }

    }


  def apply(name: String, boneyard: Deck): Player = new Player(name, fillDeck(ArrayBuffer(), boneyard))

}
