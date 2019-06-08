package drawdominoesgame

import scala.io.StdIn


object DrawDominoesApp {


  def readAnswer(message: String): String = {

    println(message)

    StdIn.readLine

  }


  def identifyAnswer(input: String, answer1: String, answer2: String): Boolean =
    if(input == answer1) true

    else false


  def showHighScore: Boolean = identifyAnswer(readAnswer(s"dominogame:~ Play game or see high scores?\n" +
    s"dominogame:~ ([-pg] for 'Play Game' and [-hs] for 'Show High Scores'): "),"-hs", "-pg")


  def runGame(game: Game): Unit = {

    if (showHighScore) {

      game.showHighScores()

      runGame(game)

    }else {

      game.gameloop()

      if (identifyAnswer(readAnswer(s"dominogame:~ Would you like to play again?\n" +
        s"dominogame:~ ([-y] for Yes and [-n] for No): "), "-y", "-n")) {

        val notFirst: Boolean = false

        runGame(game)

      } else println(s"As you wish...")

    }

  }


  def main(args: Array[String]): Unit = {

    runGame(Game())

  }

}
