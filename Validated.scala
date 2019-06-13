package drawdominoesgame


sealed trait Validated[+E, +A]

case class Valid[+A](a: A) extends Validated[Nothing, A]

case class Invalid[+E](error: E) extends Validated[E, Nothing]
