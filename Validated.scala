package drawdominoes

sealed trait Validated[+E, +A]

case class Valid[+A](a: A) extends Validated[Nothing, A]

case class Invalid[+E](error: E) extends Validated[E, Nothing]

object Invalid {

  def apply[E](error: E): Invalid[E] = Invalid(error)

}
