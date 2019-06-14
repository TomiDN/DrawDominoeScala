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

  def choiceForMovingForward(game: Game): IO[Unit] =
    if (identifyAnswer(readAnswer(s"dominogame:~ Would you like to play again?\n" +
                                  s"dominogame:~ ([-y] for Yes and [-n] for No): "), "-y", "-n")) {
      runGame(game)
    } else putStrLn(s"As you wish...")

  def showMeHS(game: Game): IO[Unit] = {
    game.showHighScores().unsafeRun()
    runGame(game)
  }

  def dontShowMeHS(game: Game): IO[Unit] = {
    game.gameloop().unsafeRun()
    choiceForMovingForward(game)
  }

  def runGame(game: Game): IO[Unit] = {
      if(showHighScore) showMeHS(game)
      else dontShowMeHS(game)
  }

  def main(args: Array[String]): Unit = {
    runGame(Game()).unsafeRun()
  }
}
