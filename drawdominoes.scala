import Player.position
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

sealed trait Tile {
  def a: Int

  def b: Int

  def print: Unit = println(s"${a}, ${b}")
}

object Tile {
  def apply(x: Int, y: Int): Tile = Tile(x, y)
}

sealed trait Player {
  def name: String

  def pile: Vector[Tile]

  def boneyardLeftovers(boneyard: Vector[Tile]): Vector[Tile] = boneyard.diff(this.pile)

  def getTile(boneyard: Vector[Tile], position: Int): Tile = boneyardLeftovers(boneyard).take(position(boneyard)).last

  def drawTile(boneyard: Vector[Tile]): Validated[String, Tile] =
    boneyardLeftovers(boneyard).length match {
      case 0 => Invalid("Boneyard is empty!")
      case _ => Valid(getTile(boneyard, position(boneyard)))
    }

  def announcePlayer: Unit = println(s"Now playing: ${this.name}")

  def printDeck: Unit =
    for (i <- this.pile) {
      i.print
    }
}

object Player {
  def position(boneyard: Vector[Tile]): Int = {
    val start = 0
    val end = boneyard.length
    val rnd = new scala.util.Random
    start + rnd.nextInt((end - start) + 1)
  }

  def fillDeck(deck: Vector[Tile], boneyard: Vector[Tile]): Vector[Tile] =
    deck.length match {
      case 7 => deck
      case _ => fillDeck(deck :+ boneyard.take(position(boneyard)).last, boneyard.diff(deck))
    }

  def apply(name: String, boneyard: Vector[Tile]): Player = Player(name, fillDeck(new Vector[Tile], boneyard))
}


sealed trait Game {
  def player1: Player
  def player2: Player
  def boneyard: Vector[Tile]
  def openends: Vector[Int]
  def last: Int

  def graphics(which: Int): Unit = which match {
    case 0 => println(s"_______________\n|               |\n|               |\n|               |\n|               |\n|               |\n|               |\n|_______________|")
    case 1 => println(s"_______________\n|               |\n|               |\n|       _       |\n|      |_|      |\n|               |\n|               |\n|_______________|")
    case 2 => println(s"_______________\n|            _  |\n|           |_| |\n|               |\n|               |\n|  _            |\n| |_|           |\n|_______________|")
    case 3 => println(s"_______________\n|            _  |\n|           |_| |\n|       _       |\n|      |_|      |\n|  _            |\n| |_|           |\n|_______________|")
    case 4 => println(s"_______________\n|  _         _  |\n| |_|       |_| |\n|               |\n|               |\n|  _         _  |\n| |_|       |_| |\n|_______________|")
    case 5 => println(s"_______________\n|  _         _  |\n| |_|       |_| |\n|       _       |\n|      |_|      |\n|  _         _  |\n| |_|       |_| |\n|_______________|")
    case 6 => println(s"_______________\n|  _         _  |\n| |_|       |_| |\n|  _         _  |\n| |_|       |_| |\n|  _         _  |\n| |_|       |_| |\n|_______________|")
    case _ => println(s"\n\n\n\n\n\n\n\n")
  }

  def passMove(currentPlayer: Int, boneyard: Vector[Tile]): Unit = graphics(last)

  def nextOpenEnd(currentPlayer: Int, boneyard: Vector[Tile]): Unit = ???

  def drawTile(currentPlayer: Int, boneyard: Vector[Tile]): Unit = {
    if (currentPlayer == 1) {
      player1.drawTile(boneyard)
    }else{
      player2.drawTile(boneyard)
    }
    graphics(last)
  }

  def pickTile(currentPlayer: Int, boneyard: Vector[Tile]): Unit = ???

  def identifyCommand(command: String, currentPlayer: Int, boneyard: Vector[Tile]):  Unit = command match {
    case "-pm" => passMove(currentPlayer, boneyard)
    case "-ne" => nextOpenEnd(currentPlayer, boneyard)
    case "-dt" => drawTile(currentPlayer, boneyard)
    case "-p" => pickTile(currentPlayer, boneyard)
    case "-q" => ()
    case _ => identifyCommand(readCommand, currentPlayer, boneyard)
  }

  def readCommand: String = {
    println(s"dominogame:~  Pick:")
    StdIn.readLine
  }

  def zeroOpenends: Unit =
    if (openends.length == 0) {
      graphics(7);
      graphics(7);
  }

  def checkPlayer(currentPlayer: Int): Unit = currentPlayer match {
    case 1 => player1.printDeck
    case 2 => player2.printDeck
  }

  def pickMoveLoop(currentPlayer: Int): Boolean = {

    let mut pickedmove: bool = false;
    let mut quit: bool = false;
    let mut first: bool = false;

    while !pickedmove && !quit {





      let mut command = String::new();



      match io::stdin().read_line(&mut command) {
        Err(e) => panic!("couldn't read the command: {}", e),
        Ok(f) => f,
      };

      if command.trim() == "-pm" {

        &self.graphics(self.last);

      }else if command.trim() == "-ne"{

        let mut iter = self.openends.iter();
        let mut again: bool = true;
        let mut c = 1;
        let len = self.openends.len();

        while again {

          let nextone = iter.next();

          if nextone == None {

            iter = self.openends.iter();
            self.last = *iter.next().unwrap();
            c = 1;

          }else{

            self.last = *nextone.unwrap();

          }

          &self.graphics(self.last);
          &self.graphics(7);

          println!("Open ends: {} (out of {})", c, len);

          if cur == 1 {

            self.player1.print();

          }else{

            self.player2.print();

          }

          println!("dominogame: ~ $ Again?([-y] - Yes, [-n] - No):");

          let mut choice = String::new();

          match io::stdin().read_line(&mut choice) {
            Err(e) => panic!("couldn't read the command: {}", e),
            Ok(f) => f,
          };

          if choice.trim() == "-n" {

            again = false;
            &self.graphics(self.last);

          }else{

            c = c + 1;

          }

        }
      }else if command.trim() == "-dp"{

        if cur == 1 {

          self.player1.draw_tile(&mut self.boneyard);

        }else{

          self.player2.draw_tile(&mut self.boneyard);

        }

        &self.graphics(self.last);

      }else if command.trim() == "-p" {

        println!("dominogame: ~ $ Pick:");

        let mut num = String::new();

        match io::stdin().read_line(&mut num) {
          Err(e) => panic!("couldn't read the number: {}", e),
          Ok(f) => f,
        };


        let mut chosen = num.trim().chars().next().unwrap().to_digit(10).unwrap() as usize;
        if num.trim().len()>1 {

          chosen = 10*chosen + num.trim().chars().next().unwrap().to_digit(10).unwrap() as usize;

        }

        &self.graphics(self.last);

        if (chosen <= self.player1.pile.len() && cur == 1) ||
        (chosen <= self.player2.pile.len() && cur == 2) {

          if self.openends.len() > 0 {

            let mut index = 0;

            for l in &self.openends {

              if l == &self.last {
                break;
              }

              index = index + 1;

            }

            if index < self.openends.len() {

              &self.openends.remove(index);

            }

          }else{

            first = true;

          }

          if cur == 1 {

            let picked = self.player1.pile.remove(chosen-1);

            if picked.a != self.last {

              &self.graphics(picked.b);
              &self.graphics(picked.a);
              self.last = picked.a;

            }else {

              &self.graphics(picked.a);
              &self.graphics(picked.b);
              self.last = picked.b;

            }

            if picked.a == picked.b || first {

              &self.openends.push(picked.a);
              &self.openends.push(picked.b);

            }

            if picked.a != self.last && picked.b != self.last {

              println!("Wrong move, but your funeral...");

            }

          }else{

            let picked = self.player2.pile.remove(chosen-1);

            if picked.a != self.last {

              &self.graphics(picked.b);
              &self.graphics(picked.a);
              self.last = picked.a;

            }else {

              &self.graphics(picked.a);
              &self.graphics(picked.b);
              self.last = picked.b;

            }

            if picked.a == picked.b || first {

              &self.openends.push(picked.a);
              &self.openends.push(picked.b);

            }

            if picked.a != self.last && picked.b != self.last {

              println!("Wrong move, but your funeral...");

            }

          }

          if !first {

            &self.openends.push(self.last);

          }

          let ten_millis = time::Duration::from_millis(1000);
          thread::sleep(ten_millis);

          pickedmove = true;

        }


      } else if command.trim() == "-q" {

        quit = true;

      }

      &self.graphics(7);

    }

    quit

  }

  def playerScheduler( quit: Boolean, currentPlayer: Int): Unit = player1.pile.length > 0 && player2.pile.length > 0 && !quit match {
    case true => currentPlayer match {
      case 1 => playerScheduler(pickMoveLoop(1), 2)
      case 2 => playerScheduler(pickMoveLoop(2), 1)
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
