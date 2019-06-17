package drawdominoesgame

import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO

class GamerUI {
  def commandLine: String = s"dominogame:~  "

  def pickLine: String = commandLine + s" Pick:"

  def instructions(): IO[Unit] = putStrLn(s"////////////////////////////////////////////////////////////////////////////////" +
                                              s"\n////////////////////////////////   Welcome to:   ///////////////////////////////" +
                                              s"\n////////////////////   [-S-Y-S-A-D-M-I-N-] DRAW DOMINOES   /////////////////////" +
                                              s"\n////////////////////////////////////////////////////////////////////////////////" +
                                              s"\n(*) Instructions:"                                                                +
                                              s"\n> [ {a number} ] - command for picking the subsequent number of a tile"           +
                                              s"\n                   in your pile to make a move with"                              +
                                              s"\n> [    -ne     ] - command for displaying another open end"                       +
                                              s"\n> [    -dp     ] - command for drawing another tile from the boneyard"            +
                                              s"\n> [    -pm     ] - command for passing a move"                                    +
                                              s"\n> [     -q     ] - command for quiting the game\n\n")

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  def createBoneyard(array: List[Tile], count: Int = 0, i: Int = 0, j: Int = 0): List[Tile] =
    if (count < 28) {
      if (i == j) {
        createBoneyard(array :+ Tile(i, j), count + 1, 0, j + 1)
      } else {
        createBoneyard(array :+ Tile(i, j), count + 1, i + 1, j)
      }
    } else array

  def fillDeck(boneyard: Deck, deck: Deck = Deck(List())): Deck =
    deck.length match {
      case 7 => deck
      case _ => boneyard.getAt(boneyard.randomPosition) match {
        case Invalid(_) => fillDeck(boneyard, deck)
        case Valid(t: Tile) =>
          fillDeck(boneyard.use(t), deck.add(t))
      }
    }

  def initialDeck: Deck = Deck(createBoneyard(List()))

  def readAnswer(message: String): IO[String] = for {
    _ <- putStrLn(message)
    input <- getStrLn
  } yield input

  def identifyAnswer(input: IO[String], answer1: String, answer2: String): Boolean =
    if (input.unsafeRun() == answer1) true
    else false

  def showHighScore: Boolean = identifyAnswer(readAnswer(commandLine + s"Play game or see high scores?\n" +
    commandLine + s"([-pg] for 'Play Game' and [-hs] for 'Show High Scores'): "), "-hs", "-pg")

  def nowPlaying(gameEntity: Game): String = gameEntity.playerInfo

  def startNewGame(): Game = {
    instructions().unsafeRun()
    newGame(readAnswer(commandLine + s"Name of Player 1:").unsafeRun(),
      readAnswer(commandLine + s"Name of Player 2:").unsafeRun(),
      initialDeck, this)
  }

  def choiceForMovingForward(): Unit =
    if (identifyAnswer(readAnswer(commandLine + s"Would you like to play again?\n" +
      commandLine + s"([-y] for Yes and [-n] for No): "), "-y", "-n")) {
      runMenu(startNewGame())
    } else putStrLn(commandLine + s"As you wish...").unsafeRun()

  def showMeHS(game: Game): Unit = {
    game.showHighScores().unsafeRun()
    runMenu(game)
  }

  def dontShowMeHS(game: Game): Unit = {
    val firstMove = true
    middleProcess(readAnswer(game.emptyScreen + nowPlaying(game) + pickLine).unsafeRun(), game, firstMove)
    choiceForMovingForward()
  }

  def runMenu(game: Game): Unit = {
    if (showHighScore) showMeHS(game)
    else dontShowMeHS(game)
  }

  def mainLoop(): Unit = runMenu(startNewGame())

  def middleProcess(command: String, gameEntity: Game, firstMove: Boolean = false): Unit =
    if (command != "-q" &&
      gameEntity.currentPlayer.pile.length > 0 &&
      gameEntity.otherPlayer.pile.length > 0) {
      doCommand(command, gameEntity, firstMove)
    } else gameEntity.endGame.unsafeRun()

  def addOpenEnd(t: Tile, openends: Deck): Deck =
    if (openends.isEmpty) {
      if (t.a == t.b) {
        openends.add(t).add(t).add(t).add(t)
      } else {
        openends.add(Tile(t.b, t.a)).add(t)
      }
    } else if (t.a == t.b) {
      openends.add(t).add(t).add(t)
    } else openends.add(t)

  def validMove(gameEntity: Game, chosen: Tile, firstMove: Boolean): Unit =
    if(firstMove) {
      val newEntity: Game = firstMoveGame(gameEntity, this, chosen)
      middleProcess(readAnswer(gameEntity.firstMove(chosen) + nowPlaying(newEntity) +  pickLine).unsafeRun(), newEntity)
    }else if (chosen.a != gameEntity.lastEnd.b && chosen.b != gameEntity.lastEnd.b)
      middleProcess(readAnswer(gameEntity.wrongMove() + pickLine).unsafeRun(), gameEntity)
    else if (chosen.a == gameEntity.lastEnd.b) {
      val newEntity: Game = gameWithMove(gameEntity, this, chosen)
      middleProcess(readAnswer(gameEntity.normalMove(chosen) + nowPlaying(newEntity) + pickLine).unsafeRun(), newEntity)
    } else {
      val newEntity: Game = gameWithMove(gameEntity, this, Tile(chosen.b, chosen.a))
      middleProcess(readAnswer(gameEntity.normalMove(chosen) + nowPlaying(newEntity)  + pickLine).unsafeRun(), newEntity)
    }

  def playMove(gameEntity: Game, chosen: Int, firstMove: Boolean): Unit =
    gameEntity.currentPlayer.pile.getAt(chosen) match {
      case Valid(piece) => validMove(gameEntity, piece, firstMove)
      case Invalid(e) => middleProcess(readAnswer(commandLine + e + '\n' + pickLine).unsafeRun(), gameEntity)
    }

  def drawPiece(gameEntity: Game, message: String): Unit =
    gameEntity.boneyard.getAt(gameEntity.boneyard.randomPosition) match {
      case Invalid(e) => middleProcess(readAnswer(commandLine + e + '\n' + pickLine).unsafeRun(), gameEntity)
      case Valid(piece) =>
        val newEntity: Game = gameWithDrawnTile(gameEntity, piece)
        middleProcess(readAnswer(message  + nowPlaying(newEntity) + pickLine).unsafeRun(), newEntity)
    }

  def tryPassMove(gameEntity: Game): Unit =
    if (!gameEntity.boneyard.isEmpty) drawPiece(gameEntity, gameEntity.unsuccessfulpassMove)
    else {
      val newEntity: Game = simpleSwitchGame(gameEntity)
      middleProcess(readAnswer(gameEntity.noMove + nowPlaying(newEntity) + pickLine).unsafeRun(), newEntity)
    }

  def showOpenEnd(gameEntity: Game): Unit = {
    val newEntity: Game = gameWithNewOpenEnd(gameEntity, gameEntity.openends.head)
    nextOpenEnd(newEntity, readAnswer(gameEntity.openEndDisplay +
      gameEntity.playerInfo + s"dominogame:~  Again?([-y] - Yes, [-n] - No):" + pickLine).unsafeRun())
  }

  def analyseOpenEnd(gameEntity: Game): Unit =
    if (!gameEntity.openends.isEmpty) showOpenEnd(gameEntity)
    else middleProcess(readAnswer(gameEntity.emptyScreen + commandLine +
      s"There are still no open ends!\n" + pickLine).unsafeRun(), gameEntity)

  def nextOpenEnd(gameEntity: Game, answer: String): Unit =
    answer match {
      case "-y" => analyseOpenEnd(gameEntity)
      case "-n"=>  middleProcess (readAnswer(pickLine).unsafeRun(), gameEntity)
    }

  def doCommand(command: String, gameEntity: Game, firstMove: Boolean): Unit =
    toInt(command) match {
      case Some(num) => playMove(gameEntity, num, firstMove)
      case None => command match {
        case "-pm" => tryPassMove(gameEntity)
        case "-ne" => nextOpenEnd(gameEntity, "-y")
        case "-dt" => drawPiece(gameEntity, gameEntity.noMove)
        case _ => middleProcess(readAnswer(pickLine).unsafeRun(), gameEntity)
      }
    }
}

object GamerUI{
  def apply(): GamerUI = new GamerUI()
}
