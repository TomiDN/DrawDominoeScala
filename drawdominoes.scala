import scala.io.StdIn

sealed trait Chain[+A] {

  def head: A

  def tail: Option[Chain[A]]

  def isEmpty: Boolean = head == None

  def +:[B >: A](front: B): Chain[B] = Append(Singleton(front), this)

  def :+[B >: A](back: B): Chain[B] = Append(this, Singleton(back))

  def ++[B >: A](right: Chain[B]): Chain[B] = Append(this, right)

  def foldLeft[B](initial: B)(f: (B, A) => B): B = this.listify match {

    case Singleton(first) => f(initial, first)

    case Append(Singleton(first), rest) => rest.foldLeft(f(initial, first): B)(f)

    case _ => sys.error("Unexpected listify format")

  }

  def reduceLeft[B >: A](f: (B, A) => B): B = this.listify match {

    case Singleton(first) => first

    case Append(Singleton(first), rest) => rest.foldLeft(first: B)(f)

    case _ => sys.error("Unexpected listify format")

  }

  def map[B](f: A => B): Chain[B] = this.listify match {

    case Singleton(first) => Singleton(f(first))

    case Append(Singleton(first), rest) => Append(Singleton(f(first)), rest.map(f))

    case _ => sys.error("Unexpected listify format")

  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = this.listify match {

    case Singleton(first) => f(first)

    case Append(Singleton(first), rest) => Append(f(first), rest.flatMap(f))

    case _ => sys.error("Unexpected listify format")

  }

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match {

    case c: Chain[_] => this.hashCode == c.hashCode

    case _ => false

  }

  override def hashCode: Int = foldLeft(0) {

    _ * 31 + _.hashCode

  }

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse

  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

  def min[B >: A](implicit order: Ordering[B]): B = this.listify match {

    case Append(Singleton(b1), Singleton(b2)) => order.min(b1, b2)

    case Append(Singleton(b), rest) => order.min(b, rest.min(order))

    case _ => sys.error("Unexpected listify format")

  }

  def max[B >: A](implicit order: Ordering[B]): B = this.listify match {

    case Append(Singleton(b1), Singleton(b2)) => order.max(b1, b2)

    case Append(Singleton(b), rest) => order.max(b, rest.max(order))

    case _ => sys.error("Unexpected listify format")

  }

  def listify: Chain[A] = this match {

    case Singleton(first) => Singleton(first)

    case Append(Singleton(first), rest) => Append(Singleton(first), rest.listify)

    case Append(left, right) =>

      val back: Chain[A] = left.tail match {

        case Some(Singleton(a)) => Append(Singleton(a), right)

        case Some(b) => Append(b, right)

        case None => right
      }

      Append(Singleton(left.head), back.listify)

    case _ => sys.error("Unexpected listify format")

  }

}

case class Singleton[+A](head: A) extends Chain[A] {

  def tail: Option[Chain[A]] = None

}

case class Append[+A](left: Chain[A], right: Chain[A]) extends Chain[A] {

  def head: A = left.head

  def tail: Option[Chain[A]] = left match {

    case Singleton(_) => Some(right)

    case _ => listify.tail

  }

}

object Chain {

  def apply[A](head: A, rest: A*): Chain[A] = rest.isEmpty match{

    case true => Singleton(head)

    case _ => Append[A](Singleton(head), apply[A](rest.head, rest.tail.head))

  }

  // Allows Chain to be used in pattern matching
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)

}

sealed trait Validated[+E, +A] {

  def isValid: Boolean = this match {

    case Invalid(_) => false

    case _ => true

  }

  def getOrElse[B >: A](default: => B): B = this match {

    case Valid(a) => a

    case _ => default

  }

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = this match {

    case v @ Valid(_) => v

    case _ => default

  }

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = this match {

    case i @ Invalid(e) => vb match {

      case Valid(_) => i

      case Invalid(c) => Invalid(e ++ c)

    }

    case Valid(a) => vb match {

      case Valid(b) => Valid((a, b))

      case i @ Invalid(_) => i

    }

  }

  def map[B](f: A => B): Validated[E, B] = this match {

    case i @ Invalid(_) => i

    case Valid(a) => Valid(f(a))

  }

  def map2[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = this match {

    case i @ Invalid(e) => vb match {

      case Valid(_) => i

      case Invalid(c) => Invalid(e ++ c)

    }

    case Valid(a) => vb match {

      case Valid(b) => Valid(f(a, b))

      case i @ Invalid(_) => i

    }

  }


  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this match {

    case Valid(a) => f(a)

    case i @ Invalid(_) => i

  }

  def fold[B](invalid: Chain[E] => B, valid: A => B): B = this match {

    case Invalid(errors) => invalid(errors)

    case Valid(a) => valid(a)

  }

  def foreach(f: A => Unit): Unit = fold(_ => (), f)

}

case class Valid[+A](a: A) extends Validated[Nothing, A]

case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid {

  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))

}

sealed trait Deck[+A] {

  def main: Vector[A]

  def used: Vector[A]

  def isEmpty: Boolean = main.diff(used).isEmpty

  def length: Int = unused.length

  def last: A = main.last

  def unused: Vector[A] = main.diff(used)

  def indexOf[A](elem: A): Int = unused.indexOf(elem)

  def add[A](back: A): Deck[A] = Deck[A](this.main :+ back, this.used)

  def use[A](back: A): Deck[A] = Deck[A](this.main, this.used :+ back)

  def find[A](piece: A): Option[A] = unused.find(_ == piece)

  def getAt[A](index: Int): Validated[String, A] =

    unused.isEmpty || (index > unused.length || index < 0) match {

      case true => Invalid(s"Invalid index!")

      case false => Valid(unused(index))

    }

  def randomPosition: Int = {

    val start = 0

    val end = length

    val rnd = new scala.util.Random

    start + rnd.nextInt((end - start) + 1)

  }

}

case class DominoDeck[+A](main: Vector[A], used: Vector[A]) extends Deck[A] 

object Deck {

  def apply[A](m: Vector[A]): Deck[A] = DominoDeck[A](m, new Vector[A])

}

class Tile(val a: Int, val b: Int) {

  def ==(t: Tile): Boolean = this.a == t.a && this.b == t.b

  def !=(t: Tile): Boolean = !(this == t)

  def print: String = s"$a, $b "

}

object Tile {

  def apply(x: Int, y: Int): Tile = new Tile(x, y)

}

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

  def announcePlayer: Unit = println(s"Now playing: ${this.name}")

  def printDeck: Unit = for (piece: Tile <- pile) println(piece.print)
}

object Player {

  def fillDeck(deck: Deck[Tile], boneyard: Deck[Tile]): Deck[Tile] =

    deck.length match {

      case 7 => deck

      case _ => boneyard.use(boneyard.randomPosition) match {

        case Invalid(_) => fillDeck(deck, boneyard)

        case Valid(t: Tile) => fillDeck(deck.add(t), boneyard)

      }

    }

  def apply(name: String, boneyard: Deck[Tile]): Player = new Player(name, fillDeck(Deck[Tile](new Vector[Tile]), boneyard))

}


class Game(val player1: Player, 
           val player2: Player, 
           val boneyard: Deck[Tile], 
           val openends: Deck[Tile] = new Deck[Tile](new Vector[Tile], new Vector[tile]), 
           val lastends: Deck[Tile] = new Deck[Tile](new Vector[Tile], new Vector[tile])) {

  def toInt(s: String): Option[Int] = {

    try {

      Some(s.toInt)

    } catch {

      case _: Exception => None

    }

  }

  def graphics(which: Int): String = which match {

    case 0 => s"_______________" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|_______________|\n"

    case 1 => s"_______________" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|       _       |" +
           s"\n|      |_|      |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|_______________|\n"

    case 2 => s"_______________" +
           s"\n|            _  |" +
           s"\n|           |_| |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|  _            |" +
           s"\n| |_|           |" +
           s"\n|_______________|\n"

    case 3 => s"_______________" +
           s"\n|            _  |" +
           s"\n|           |_| |" +
           s"\n|       _       |" +
           s"\n|      |_|      |" +
           s"\n|  _            |" +
           s"\n| |_|           |" +
           s"\n|_______________|\n"

    case 4 => s"_______________" + 
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|               |" +
           s"\n|               |" +
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|_______________|\n"

    case 5 => s"_______________" +
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|       _       |" +
           s"\n|      |_|      |" +
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|_______________|\n"

    case 6 => s"_______________" +
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|  _         _  |" +
           s"\n| |_|       |_| |" +
           s"\n|_______________|\n"

    case _ => s"\n\n\n\n\n\n\n\n"

  }

  def lastEnd: Tile = lastends.last

  def passMove: String = graphics(lastEnd.b)

  def newLast(newLast: Tile): Tile = this.lastends.add(newLast).last

  def showNextOpenEnd: String = openends.indexOf(lastEnd) match {

    case -1 => graphics(lastEnd.b) + graphics(7) + s"This is the only open end"

    case n => openends.getAt(n) match {

      case Invalid(a) => graphics(lastEnd.b) + graphics(7) + a

      case Valid(t: Tile) => graphics(newLast(t).b) + graphics(7) + s"Open ends: ${n+1} (out of $openends.length)"

    }

  }

  def nextOpenEndMenu(currentPlayer: Player): Unit = {

    println(showNextOpenEnd)

    currentPlayer.printDeck

    println(s"dominogame:~  Again?([-y] - Yes, [-n] - No):")

  }

  def nextOpenEnd(currentPlayer: Player): String = {

    nextOpenEndMenu(currentPlayer)

    StdIn.readLine match {

      case "-n" => graphics(lastEnd.b)

      case _ => nextOpenEnd(currentPlayer)

    }

  }

  def wasTileDrawn(currentPlayer: Player): Validated[String, Deck[Tile]] = currentPlayer.drawTileFromBN(boneyard)

  def drawTile(currentPlayer: Player): String = wasTileDrawn(currentPlayer) match {

    case Invalid(Chain(a)) => a

    case Valid(_) => graphics(lastEnd.b)

  }

  def pickTile(currentPlayer: Player): String = toInt(readCommand) match {

    case None => pickTile(currentPlayer)

    case Some(num) => currentPlayer.drawTileFromPile(num) match {

      case Invalid(_) => pickTile(currentPlayer)

      case Valid(t) => openends.isEmpty match {

        case true =>

          openends.add(t)

          graphics(t.b)

        case false => t.a != lastEnd.b && t.b != lastEnd.b match {

          case true => pickTile(currentPlayer)

          case false => t.a == lastEnd.b match {

            case true =>

              openends.use(lastEnd)

              openends.add(t)

              graphics(t.a)+graphics(t.b)

            case false =>

              openends.use(lastEnd)

              openends.add(Tile (t.b, t.a) )

              graphics(t.b)+graphics(t.a)

          }

        }

      }

    }

  }

  def identifyCommand(currentPlayer: Player):  String = readCommand match {

    case "-pm" => passMove

    case "-ne" => nextOpenEnd(currentPlayer)

    case "-dt" => drawTile(currentPlayer)

    case "-p" => pickTile(currentPlayer)

    case "-q" => ""

    case _ => identifyCommand(currentPlayer)

  }

  def readCommand: String = {

    println(s"dominogame:~  Pick:")

    StdIn.readLine

  }


  def zeroOpenends: Unit = println(graphics(7) + graphics(7))

  def playerInfo(currentPlayer: Player): Unit = {

    currentPlayer.announcePlayer

    currentPlayer.printDeck

  }

  def pickMoveLoop(currentPlayer: Player): Boolean = identifyCommand(currentPlayer) match {

    case "" => false

    case visualisation: String => 

      println(visualisation)

      Thread.sleep(1000)

      println(graphics(7))

      pickMoveLoop(currentPlayer)

  }

  def playerScheduler( quit: Boolean, currentPlayer: Int): Unit = player1.pile.length > 0 && player2.pile.length > 0 && !quit match {

    case true => currentPlayer match {

      case 1 => playerScheduler(pickMoveLoop(player1), 2)

      case 2 => playerScheduler(pickMoveLoop(player2), 1)

    }

    case false => println(s"dominogame:~ Calculating results...")

  }

  def gameloop: Unit = {

    zeroOpenends

    playerScheduler(false, 1)

    if (player1.pile.length == 0) {

      println(s"dominogame:~ ${player1.name} IS THE WINNER")

    }else if (player2.pile.length == 0) {

      println(s"dominogame:~ ${player2.name} IS THE WINNER")

    }

    println(s"\ndominogame:~ GOOD GAME, BYE!" + graphics(7))

  }

}

object Game {

  def createBoneyard(vec: Vector[Tile], count: Int = 0, i: Int = 0, j: Int = 0): Vector[Tile] =

    if (count < 28){

      if (i == j) {

        createBoneyard(vec :+ Tile(i, j), count + 1, i, j + 1)

      }else{

        createBoneyard(vec :+ Tile(i, j), count + 1, i+1, j)

      }

    } else {

      vec :+ Tile(i, j)

    }

  def instructions: Unit = println(s"////////////////////////////////////////////////////////////////////////////////" +
    s"\n////////////////////////////////   Welcome to:   ///////////////////////////////" +
    s"\n////////////////////   [-S-Y-S-A-D-M-I-N-] DRAW DOMINOES   /////////////////////" +
    s"\n////////////////////////////////////////////////////////////////////////////////" +
    s"\n(*) Instructions:\n> [ -p ] - command for picking a tile to make your" +
    s"\nmove with and you'll be asked to pick it's subsequent number in your pile" +
    s"\n> [ -ne ] - command for displaying another open end" +
    s"\n> [ -dp ] - command for drawing another tile from" +
    s"\n the boneyard" +
    s"\n> [ -pm ] - command for passing a move" +
    s"\n> [ -q ] - command for quiting the game\n\n")

  def readPlayerName(currentPlayer: Int): String = {

    println(s"dominogame:~ Name of Player ${currentPlayer}:")

    StdIn.readLine

  }

  def apply(): Game = {

      val boneyard: Deck[Tile] = new Deck[Tile](createBoneyard(new Vector[Tile]))

      instructions

      new Game(Player(readPlayerName(1), boneyard), Player(readPlayerName(2), boneyard), boneyard)

  }

}


object DrawDominoApp {

  def readAnswer: String = {

    println(s"dominogame:~ Would you like to play again?\n" +
            s"\ndominogame:~ ([-y] for Yes and [-n] for No): ")

    StdIn.readLine

  }

  def identifyAnswer: Boolean = readAnswer match {

    case "-y" => true

    case "-n" => false

  }

  def runGame(game: Game): Unit = {

    game.gameloop

    if (identifyAnswer) {

      runGame(game)

    } else println(s"As you wish...")

  }

  def main(args: Array[String]): Unit = {

    runGame(Game())

  }

}
