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

object Validated {

  implicit class ValidatedTuple2[EE, A, B](val tuple: (Validated[EE, A], Validated[EE, B])) extends AnyVal {
    def zip: Validated[EE, (A, B)] = tuple._1.zip(tuple._2)
    def zipMap[R](f: (A, B) => R): Validated[EE, R] = tuple._1.map2(tuple._2)(f)
  }

  implicit class ValidatedTuple3[EE, A, B, C](val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C])) extends AnyVal {
    def zip: Validated[EE, (A, B, C)] = tuple match {
      case (Invalid(e), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Invalid(u)) => Invalid(e ++ u)
      case (i @ Invalid(_), Valid(_), Valid(_)) => i
      case (Valid(_), i @ Invalid(_), Valid(_)) => i
      case (Valid(_), Valid(_), i @ Invalid(_)) => i
      case (Valid(a), Valid(b), Valid(c)) => Valid(a, b, c)
    }
    def zipMap[R](f: (A, B, C) => R): Validated[EE, R] = tuple match {
      case (Invalid(e), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Invalid(u)) => Invalid(e ++ u)
      case (i @ Invalid(_), Valid(_), Valid(_)) => i
      case (Valid(_), i @ Invalid(_), Valid(_)) => i
      case (Valid(_), Valid(_), i @ Invalid(_)) => i
      case (Valid(a), Valid(b), Valid(c)) => Valid(f(a, b, c))
    }
  }

  implicit class ValidatedTuple4[EE, A, B, C, D]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D)] = tuple match {
      case (Invalid(e), Invalid(u), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Invalid(e), Invalid(u)) => Invalid(e ++ u)
      case (i @ Invalid(_), Valid(_), Valid(_), Valid(_)) => i
      case (Valid(_), i @ Invalid(_), Valid(_), Valid(_)) => i
      case (Valid(_), Valid(_), i @ Invalid(_), Valid(_)) => i
      case (Valid(_), Valid(_), Valid(_), i @ Invalid(_)) => i
      case (Valid(a), Valid(b), Valid(c), Valid(d)) => Valid(a, b, c, d)
    }
    def zipMap[R](f: (A, B, C, D) => R): Validated[EE, R] = tuple match {
      case (Invalid(e), Invalid(u), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Invalid(e), Invalid(u)) => Invalid(e ++ u)
      case (i @ Invalid(_), Valid(_), Valid(_), Valid(_)) => i
      case (Valid(_), i @ Invalid(_), Valid(_), Valid(_)) => i
      case (Valid(_), Valid(_), i @ Invalid(_), Valid(_)) => i
      case (Valid(_), Valid(_), Valid(_), i @ Invalid(_)) => i
      case (Valid(a), Valid(b), Valid(c), Valid(d)) => Valid(f(a, b, c, d))
    }
  }

  implicit class ValidatedTuple5[EE, A, B, C, D, E]
  (val tuple: (Validated[EE, A], Validated[EE, B], Validated[EE, C], Validated[EE, D], Validated[EE, E])) extends AnyVal {
    def zip: Validated[EE, (A, B, C, D, E)] = tuple match {
      case (Invalid(e), Invalid(u), Invalid(r), Invalid(o), Invalid(p)) => Invalid(e ++ u ++ r ++ o ++ p)
      case (Invalid(e), Invalid(u), Invalid(r), Invalid(o), Valid(_)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Invalid(r), Valid(_), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Valid(_), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Valid(_), Invalid(u), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Valid(_), Invalid(e), Invalid(u), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Invalid(r), Valid(_), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Invalid(u), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Invalid(u), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Valid(_), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Valid(_), Invalid(e), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Invalid(u), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Invalid(u), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Valid(_), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Valid(_), Invalid(e), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Invalid(e), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Invalid(e), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Valid(_), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Invalid(u), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Valid(_), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Valid(_), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Invalid(u), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Invalid(u), Valid(_), Invalid(e), Valid(_)) => Invalid(e ++ u)
      case (i@Invalid(_), Valid(_), Valid(_), Valid(_), Valid(_)) => i
      case (Valid(_), i@Invalid(_), Valid(_), Valid(_), Valid(_)) => i
      case (Valid(_), Valid(_), i@Invalid(_), Valid(_), Valid(_)) => i
      case (Valid(_), Valid(_), Valid(_), i@Invalid(_), Valid(_)) => i
      case (Valid(_), Valid(_), Valid(_), Valid(_), i@Invalid(_)) => i
      case (Valid(a), Valid(b), Valid(c), Valid(d), Valid(e)) => Valid(a, b, c, d, e)
    }
    def zipMap[R](f: (A, B, C, D, E) => R): Validated[EE, R] = tuple match {
      case (Invalid(e), Invalid(u), Invalid(r), Invalid(o), Invalid(p)) => Invalid(e ++ u ++ r ++ o ++ p)
      case (Invalid(e), Invalid(u), Invalid(r), Invalid(o), Valid(_)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Invalid(r), Valid(_), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Valid(_), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Valid(_), Invalid(u), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Valid(_), Invalid(e), Invalid(u), Invalid(r), Invalid(o)) => Invalid(e ++ u ++ r ++ o)
      case (Invalid(e), Invalid(u), Invalid(r), Valid(_), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Invalid(u), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Invalid(u), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Invalid(e), Valid(_), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Valid(_), Valid(_), Invalid(e), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Invalid(u), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Invalid(u), Valid(_), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Valid(_), Valid(_), Invalid(u), Invalid(r)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Invalid(r), Valid(_)) => Invalid(e ++ u ++ r)
      case (Invalid(e), Invalid(u), Valid(_), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Valid(_), Invalid(e), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Invalid(e), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Valid(_), Invalid(e), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Valid(_), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Valid(_), Invalid(e), Invalid(u), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Valid(_), Invalid(u), Valid(_)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Valid(_), Valid(_), Invalid(u)) => Invalid(e ++ u)
      case (Invalid(e), Valid(_), Invalid(u), Valid(_), Valid(_)) => Invalid(e ++ u)
      case (Valid(_), Invalid(u), Valid(_), Invalid(e), Valid(_)) => Invalid(e ++ u)
      case (i@Invalid(_), Valid(_), Valid(_), Valid(_), Valid(_)) => i
      case (Valid(_), i@Invalid(_), Valid(_), Valid(_), Valid(_)) => i
      case (Valid(_), Valid(_), i@Invalid(_), Valid(_), Valid(_)) => i
      case (Valid(_), Valid(_), Valid(_), i@Invalid(_), Valid(_)) => i
      case (Valid(_), Valid(_), Valid(_), Valid(_), i@Invalid(_)) => i
      case (Valid(a), Valid(b), Valid(c), Valid(d), Valid(e)) => Valid(f(a, b, c, d, e))
    }
  }
}

sealed trait Deck[+A] {
  def main: Vector[A]

  def used: Vector[A]

  def isEmpty: Boolean = main.diff(used).isEmpty

  def length: Int = unused.length

  def last: A = main.last

  def unused: Vector[A] = main.diff(used)

  def indexOf(elem: A): Int = unused.indexOf(elem)

  def add[A](back: A): Deck[A] = Deck[A](this.main :+ back, this.used)

  def use[A](back: A): Deck[A] = Deck[A](this.main, this.used :+ back)

  def find[A](piece: A): Option[A] = unused.find(_ == piece)

  def getAt[A](index: Int): Validated[Int, A] =
    unused.isEmpty || (index > unused.length || index < 0) match {
      case true => Invalid(index)
      case false => Valid(unused(index))
    }

  def randomPosition: Int = {
    val start = 0
    val end = length
    val rnd = new scala.util.Random
    start + rnd.nextInt((end - start) + 1)
  }
}


object Deck {
  def apply[A](m: Vector[A], u: Vector[A]): Deck[A] = Deck[A](m, u)
}

sealed trait Tile {
  def a: Int

  def b: Int

  def ==(t: Tile): Boolean = this.a == t.a && this.b == t.b

  def !=(t: Tile): Boolean = !(this == t)

  def print: String = s"$a, $b "
}

object Tile {
  def apply(x: Int, y: Int): Tile = Tile(x, y)
}

sealed trait Player {
  def name: String

  def pile: Deck[Tile]

  def getTileFromBN(boneyard: Deck[Tile], position: Int): Deck[Tile] = boneyard.getAt(position) match {
    case Invalid(_) => pile
    case Valid(t) => pile.add(t)
  }

  def drawTileFromBN(boneyard: Deck[Tile]): Validated[String, Deck[Tile]] =
    boneyard.length match {
      case 0 => Invalid("Boneyard is empty!")
      case _ => Valid(getTileFromBN(boneyard, boneyard.randomPosition))
    }

  def drawTileFromPile(index: Int): Validated[String, Tile] =  pile.getAt(index) match {
      case Invalid(_) => Invalid("Invalid index!")
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

  def apply(name: String, boneyard: Deck[Tile]): Player = Player(name, fillDeck(Deck[Tile](new Vector[Tile], new Vector[Tile]), boneyard))
}


sealed trait Game {
  def player1: Player

  def player2: Player

  def boneyard: Deck[Tile]

  def openends: Deck[Tile]

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  def graphics(which: Int): String = which match {
    case 0 => s"_______________\n|               |\n|               |\n|               |\n|               |\n|               |\n|               |\n|_______________|\n"
    case 1 => s"_______________\n|               |\n|               |\n|       _       |\n|      |_|      |\n|               |\n|               |\n|_______________|\n"
    case 2 => s"_______________\n|            _  |\n|           |_| |\n|               |\n|               |\n|  _            |\n| |_|           |\n|_______________|\n"
    case 3 => s"_______________\n|            _  |\n|           |_| |\n|       _       |\n|      |_|      |\n|  _            |\n| |_|           |\n|_______________|\n"
    case 4 => s"_______________\n|  _         _  |\n| |_|       |_| |\n|               |\n|               |\n|  _         _  |\n| |_|       |_| |\n|_______________|\n"
    case 5 => s"_______________\n|  _         _  |\n| |_|       |_| |\n|       _       |\n|      |_|      |\n|  _         _  |\n| |_|       |_| |\n|_______________|\n"
    case 6 => s"_______________\n|  _         _  |\n| |_|       |_| |\n|  _         _  |\n| |_|       |_| |\n|  _         _  |\n| |_|       |_| |\n|_______________|\n"
    case _ => s"\n\n\n\n\n\n\n\n"
  }

  def passMove(last: Tile): String = graphics(last.b)

  def newLast(last: Tile, newLast: Tile): Tile = ???

  def showNextOpenEnd(last: Tile): String = openends.indexOf(last) match {
      case -1 => graphics(last.b) + graphics(7)
      case n => openends.getAt(n) match {
        case Invalid(_) => graphics(last.b) + graphics(7)
        case Valid(t) => graphics(t.b) + graphics(7) 
      }
    }

  def nextOpenEndMessage(last: Tile): String = 
    showNextOpenEnd(last) + 
    s"Open ends: $(n+1) (out of $openends.length)"
    
  def nextOpenEndMenu(currentPlayer: Player, last: Tile): Unit = {
    println(nextOpenEndMessage(last))
    currentPlayer.printDeck 
    println(s"dominogame: ~ $ Again?([-y] - Yes, [-n] - No):")
  }

  def nextOpenEnd(currentPlayer: Player, last: Tile): String = {
    nextOpenEndMenu(currentPlayer, last)
    StdIn.readLine match {
      case "-n" => graphics(last.b)
      case _ => nextOpenEnd(currentPlayer, last)
    }
  }

  def wasTileDrawn(currentPlayer: Player, boneyard: Deck[Tile]): Validated[String, Deck[Tile]] = currentPlayer.drawTileFromBN(boneyard)

  def drawTile(currentPlayer: Player, last: Tile, boneyard: Deck[Tile]): String = wasTileDrawn(currentPlayer, boneyard) match {
    case Invalid(Chain(a)) => a
    case Valid(_) => graphics(last.b)
  }

  def pickTile(currentPlayer: Player, last: Tile, boneyard: Deck[Tile]): String = toInt(readCommand) match {
    case None => pickTile(currentPlayer, last, boneyard)
    case Some(num) => currentPlayer.drawTileFromPile(num) match {
      case Invalid(_) => pickTile(currentPlayer, last, boneyard)
      case Valid(t) => openends.isEmpty match {
        case true => {
          openends.add(t)
          graphics(t.b)
        }
        case false => t.a != last.b && t.b != last.b match {
          case true => pickTile(currentPlayer, last, boneyard)
          case false => t.a == last.b match {
            case true => {
              openends.use(last)
              openends.add(t)
              graphics(t.a)+graphics(t.b)
            }
            case false => {
              openends.use(last)
              openends.add(Tile (t.b, t.a) )
              graphics(t.b)+graphics(t.a)
            }
          }
        }
      }
    }
  }

  def identifyCommand(currentPlayer: Player, last: Tile, boneyard: Deck[Tile]):  String = readCommand match {
    case "-pm" => passMove(last)
    case "-ne" => nextOpenEnd(currentPlayer, last, boneyard)
    case "-dt" => drawTile(currentPlayer, last, boneyard)
    case "-p" => pickTile(currentPlayer, last, boneyard)
    case "-q" => ""
    case _ => identifyCommand(currentPlayer, last, boneyard)
  }

    def readCommand: String = {
      println(s"dominogame:~  Pick:")
      StdIn.readLine
    }

    def zeroOpenends: String =
      if (openends.isEmpty) {
        graphics(7) + graphics(7)
      }else {
        ""
      }

    def playerInfo(currentPlayer: Player): Unit = {
      currentPlayer.announcePlayer
      currentPlayer.printDeck
    }

    def pickMoveLoop(currentPlayer: Player): Boolean = ???

    def playerScheduler( quit: Boolean, currentPlayer: Int): Unit = player1.pile.length > 0 && player2.pile.length > 0 && !quit match {
      case true => currentPlayer match {
        case 1 => playerScheduler(pickMoveLoop(player1), 2)
        case 2 => playerScheduler(pickMoveLoop(player2), 1)
      }
      case false => println(s"dominogame:~ Calculating results...")
    }

    def gameloop: Unit = {

      playerScheduler(false, 1)

      if (player1.pile.length == 0) {

        println(s"dominogame:~ ${player1.name} IS THE WINNER")

      }else if (player2.pile.length == 0) {

        println(s"dominogame:~ ${player2.name} IS THE WINNER")

      }

      println("\ndominogame:~ GOOD GAME, BYE!")

      graphics(7)
    }
  }

  object Game {
    def apply -> Self {

      let mut b = Vec::new();
      let mut k = 0;
      let mut i = 0;
      let mut j = 0;

      while k < 28 {

        let t = Tile::new(i,j);
        b.push(t);

        k = k + 1;

        if i==j {

          i = 0;
          j = j+1;

        }else {

          i = i + 1;

        }

      }

      println!("////////////////////////////////////////////////////////////////////////////////
        ////////////////////////////////   Welcome to:   ///////////////////////////////
        ////////////////////   [-S-Y-S-A-D-M-I-N-] DRAW DOMINOES   /////////////////////
        ////////////////////////////////////////////////////////////////////////////////
        (*) Instructions:
        $> [ -p ] - command for picking a tile to make your
        move with and you'll be asked to pick it's subsequent
        number in your pile
        $> [ -ne ] - command for displaying another open end
        $> [ -dp ] - command for drawing another tile from
        the boneyard
        $> [ -pm ] - command for passing a move
      $> [ -q ] - command for quiting the game");

        println!();println!();

        let mut name1 = String::new();

        println!("dominogame: ~ $ Name of Player1:");

        match io::stdin().read_line(&mut name1) {
        Err(e) => panic!("couldn't read the name of player1: {}", e),
        Ok(f) => f,
        };

        let mut name2 = String::new();

        println!("dominogame: ~ $ Name of Player2:");

        match io::stdin().read_line(&mut name2) {
        Err(e) => panic!("couldn't read the name of player2: {}", e),
        Ok(f) => f,
        };

        let p1 = Player::new(&name1.trim(),&mut b);
        let p2 = Player::new(&name2.trim(),&mut b);

        let g = Game {
        player1: p1,
        player2: p2,
        boneyard: b,
        openends: Vec::new(),
        last: 7,
        };

        g

        }





        }

        fn main(){

        let mut again: bool = true;

        while again {

        let mut gameplay = Game::new();

        gameplay.gameloop();

        let mut command = String::new();

        println!("dominogame: ~ $ Would you like to play again?");
        println!();
        println!("dominogame: ~ $ ([-y] for Yes and [-n] for No): ");

        match io::stdin().read_line(&mut command) {
        Err(e) => panic!("couldn't read the command: {}", e),
        Ok(f) => f,
        };

        if command.trim() == "-n" {

        again = false;

        } else if command.trim() == "-y" {
        }else{

        again = false;

        }

        }

        }

        }
