package drawdominoesgame

import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO

object DrawDominoesApp {
  def readAnswer(message: String): IO[String] = for {
    _ <- putStrLn(message)
    input <- getStrLn
  } yield input

  def identifyAnswer(input: IO[String], answer1: String, answer2: String): Boolean =
    if(input.unsafeRun() == answer1) true
    else false

  def showHighScore: Boolean = identifyAnswer(readAnswer(s"dominogame:~ Play game or see high scores?\n" +
    s"dominogame:~ ([-pg] for 'Play Game' and [-hs] for 'Show High Scores'): "),"-hs", "-pg")

  def runGame(game: Game): IO[Unit] = {
    if (showHighScore) {
      game.showHighScores().unsafeRun()
      runGame(game)
    }else {
      game.gameloop().unsafeRun()
      if (identifyAnswer(readAnswer(s"dominogame:~ Would you like to play again?\n" +
        s"dominogame:~ ([-y] for Yes and [-n] for No): "), "-y", "-n")) {
        runGame(game)
      } else putStrLn(s"As you wish...")
    }
  }

  def main(args: Array[String]): Unit = {
    runGame(Game()).unsafeRun()
  }
}
