package drawdominoes

import scala.collection.mutable.ArrayBuffer

class Deck[+A](val buffer: ArrayBuffer[A]) {

  def isEmpty: Boolean = main.diff(used).isEmpty

  def length: Int = unused.length

  def last: A = main.last

  def indexOf[B](elem: B): Int = unused.indexOf(elem)

  def access: ArrayBuffer[A] = this.buffer

  def getAt[E >: A](index: Int): Validated[String, A] =

    if (buffer.isEmpty || (index > buffer.length || index < 0))

      Invalid(s"Invalid index!")

    else Valid(buffer(index))


  def randomPosition: Int = {

    val start = 0

    val end = length

    val rnd = new scala.util.Random

    start + rnd.nextInt((end - start) + 1)

  }

}

object Deck {

  def apply[A](b: ArrayBuffer[A]): Deck[A] = new Deck[A](b)

}
