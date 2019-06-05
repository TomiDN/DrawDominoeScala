package drawdominoes

import scala.io.StdIn

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

    game.gameloop()

    if (identifyAnswer) {

      runGame(game)

    } else println(s"As you wish...")

  }

  def main(args: Array[String]): Unit = {

    runGame(Game())

  }

}
