package drawdominoes

class Deck[+A](val main: List[A], val used: List[A]) {

  def isEmpty: Boolean = main.diff(used).isEmpty

  def length: Int = unused.length

  def last: A = main.last

  def head: A = unused.head

  def tail: List[A] = unused.tail

  def unused: List[A] = main.diff(used)

  def indexOf[B](elem: B): Int = unused.indexOf(elem)

  def add[B >: A](back: B): Deck[B] = Deck[B](this.main :+ back, this.used)

  def use[B >: A](back: B): Deck[B] = Deck[B](this.main, this.used :+ back)

  def find[B](piece: B): Option[A] = unused.find(_ == piece)

  def getAt[E >: A](index: Int): Validated[String, A] =

    if (unused.isEmpty || (index > unused.length || index < 0))

      Invalid(s"Invalid index!")

    else Valid(unused(index))


  def randomPosition: Int = {

    val start = 0

    val end = length

    val rnd = new scala.util.Random

    start + rnd.nextInt((end - start) + 1)

  }

}

object Deck {

  def apply[A](m: List[A], u: List[A]): Deck[A] = new Deck[A](m, u)

}
