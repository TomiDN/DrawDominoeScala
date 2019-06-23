package drawdominoesgame

import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO


class GamerUI {
  val first: Boolean = true

  val yes: String = "-y"

  val no: String = "-n"

  val highScores: String = "-hs"

  val playGame: String = "-pg"

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  def readAnswer(message: String): IO[String] = for {
    _ <- putStrLn(message)
    input <- getStrLn
  } yield input

  def identifyAnswer(input: IO[String], answer1: String, answer2: String): Boolean =
    if (input.unsafeRun() == answer1) true
    else false

  def nowPlaying(gameEntity: Game): String = gameEntity.playerInfo

  def validMove(gameEntity: Game, chosen: Tile, firstMove: Boolean): Unit =
    if(firstMove) {
      val newEntity: Game = firstMoveGame(gameEntity, chosen)
      middleProcess(readAnswer(gameEntity.firstMove(chosen) + nowPlaying(newEntity) +  Game.pickLine).unsafeRun(), newEntity)
    } else if (chosen.a != gameEntity.lastEnd.b && chosen.b != gameEntity.lastEnd.b)
      middleProcess(readAnswer(gameEntity.wrongMove + Game.pickLine).unsafeRun(), gameEntity)
    else if (chosen.a == gameEntity.lastEnd.b) {
      val newEntity: Game = gameWithMove(gameEntity, chosen)
      middleProcess(readAnswer(gameEntity.normalMove(chosen) + nowPlaying(newEntity) + Game.pickLine).unsafeRun(), newEntity)
    } else {
      val newEntity: Game = gameWithMove(gameEntity, Tile(chosen.b, chosen.a))
      middleProcess(readAnswer(gameEntity.normalMove(chosen) + nowPlaying(newEntity)  + Game.pickLine).unsafeRun(), newEntity)
    }

  def playMove(gameEntity: Game, chosen: Int, firstMove: Boolean): Unit =
    gameEntity.currentPlayer.pile.getAt(chosen) match {
      case Valid(piece) => validMove(gameEntity, piece, firstMove)
      case Invalid(e) => middleProcess(readAnswer(Game.commandLine + e + Game.pickLine).unsafeRun(), gameEntity)
    }

  def drawPiece(gameEntity: Game, message: String): Unit = {
    if(gameEntity.boneyard.isEmpty){
      middleProcess(readAnswer(Game.commandLine + s"Boneyard is empty!\n" + Game.pickLine).unsafeRun(), gameEntity)
    }else {
      val newEntity: Game = gameWithDrawnTile(gameEntity, gameEntity.boneyard.getRandomElement)
      middleProcess(readAnswer(message + nowPlaying(newEntity) + Game.pickLine).unsafeRun(), newEntity)
    }
  }

  def tryPassMove(gameEntity: Game): Unit =
    if (!gameEntity.boneyard.isEmpty) drawPiece(gameEntity, gameEntity.unsuccessfulpassMove)
    else {
      val newEntity: Game = simpleSwitchGame(gameEntity)
      middleProcess(readAnswer(gameEntity.noMove + nowPlaying(newEntity) + Game.pickLine).unsafeRun(), newEntity)
    }

  def showNextOpenEnd(gameEntity: Game): Boolean = identifyAnswer(readAnswer(gameEntity.openEndDisplay +
    nowPlaying(gameEntity) + Game.commandLine + s"Again?([$yes] - Yes, [$no] - No):" + Game.pickLine), yes, no)

  def showOpenEnd(gameEntity: Game): Unit = {
    val newEntity: Game = gameWithNewOpenEnd(gameEntity, gameEntity.openends.head)
    nextOpenEnd(newEntity, showNextOpenEnd(newEntity))
  }

  def analyseOpenEnd(gameEntity: Game): Unit =
    if (!gameEntity.openends.isEmpty) showOpenEnd(gameEntity)
    else middleProcess(readAnswer(gameEntity.emptyScreen + Game.commandLine +
      s"There are still no open ends!\n" + nowPlaying(gameEntity) + Game.pickLine).unsafeRun(), gameEntity)

  def nextOpenEnd(gameEntity: Game, answer: Boolean): Unit =
    if (answer)  analyseOpenEnd(gameEntity)
    else  middleProcess(readAnswer(Game.pickLine).unsafeRun(), gameEntity)

  def otherCommands(command: String, gameEntity: Game, firstMove: Boolean): Unit =
    if      (command == Game.passMove) tryPassMove(gameEntity)
    else if (command == Game.nextMove) nextOpenEnd(gameEntity, first)
    else if (command == Game.drawTile) drawPiece(gameEntity, gameEntity.noMove)
    else     middleProcess(readAnswer(Game.commandLine + s"Invalid command!\n" + Game.pickLine).unsafeRun(), gameEntity)

  def doCommand(command: String, gameEntity: Game, firstMove: Boolean): Unit =
    toInt(command) match {
      case Some(num) => playMove(gameEntity, num, firstMove)
      case None => otherCommands(command, gameEntity, firstMove)
    }

  def choiceForMovingForward(): Unit =
    if (identifyAnswer(readAnswer(Game.commandLine + s"Would you like to play again?\n" +
      Game.commandLine + s"([$yes] for Yes and [$no] for No): "), yes, no)) {
      runMenu(initializeGame)
    } else putStrLn(Game.commandLine + s"As you wish...").unsafeRun()

  def middleProcess(command: String, gameEntity: Game, firstMove: Boolean = false): Unit =
    if (command != Game.quitGame && gameEntity.currentPlayer.pile.length > 0 && gameEntity.otherPlayer.pile.length > 0) {
      doCommand(command, gameEntity, firstMove)
    } else gameEntity.endGame.unsafeRun()

  def dontShowMeHS(game: Game): Unit = {
    middleProcess(readAnswer(game.emptyScreen + nowPlaying(game) + Game.pickLine).unsafeRun(), game, first)
    choiceForMovingForward()
  }

  def showMeHS(game: Game): Unit = {
    game.showHighScores().unsafeRun()
    runMenu(game)
  }

  def showHighScore: Boolean = identifyAnswer(readAnswer(Game.commandLine + s"Play game or see high scores?\n" +
    Game.commandLine + s"([$playGame] for 'Play Game' and [$highScores] for 'Show High Scores'): "), highScores, playGame)

  def runMenu(game: Game): Unit = {
    if (showHighScore) showMeHS(game)
    else dontShowMeHS(game)
  }

  def initializeGame: Game =
    Game.startNewGame(readAnswer(Game.instructions + Game.commandLine + s"Name of Player 1:").unsafeRun(),
                      readAnswer(Game.commandLine + s"Name of Player 2:").unsafeRun())

  def mainLoop(): Unit =
    runMenu(initializeGame)
}

object GamerUI{
  def apply(): GamerUI = new GamerUI()
