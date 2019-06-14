package drawdominoesgame

import scala.collection.mutable.ArrayBuffer
import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO


class Game(val player1: Player,
           val player2: Player,
           val boneyard: Deck,
           val openends: Deck = Deck(ArrayBuffer())) {
  def highScoreSystem: HighScoring = HighScoring("DDHighScores.txt",
      s"[-S-Y-S-A-D-M-I-N-] DRAW DOMINOES HIGH SCORES\n\n" +
      s"format is:\n" +
      s"[player name] - [count of tiles (higher than 0) that were left in the boneyard]:\n\n",
      s"Still no high scores... Noobs...")

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _: Exception => None
    }
  }

  def graphics(which: Int): String = which match {
    case 0 =>  s"_______________"  +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|_______________|\n"
    case 1 =>  s"_______________"  +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|       _       |" +
            s"\n|      |_|      |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|_______________|\n"
    case 2 =>  s"_______________"  +
            s"\n|            _  |" +
            s"\n|           |_| |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|  _            |" +
            s"\n| |_|           |" +
            s"\n|_______________|\n"
    case 3 =>  s"_______________"  +
            s"\n|            _  |" +
            s"\n|           |_| |" +
            s"\n|       _       |" +
            s"\n|      |_|      |" +
            s"\n|  _            |" +
            s"\n| |_|           |" +
            s"\n|_______________|\n"
    case 4 =>  s"_______________"  +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|               |" +
            s"\n|               |" +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|_______________|\n"
    case 5 =>  s"_______________"  +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|       _       |" +
            s"\n|      |_|      |" +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|_______________|\n"
    case 6 =>  s"_______________"  +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|  _         _  |" +
            s"\n| |_|       |_| |" +
            s"\n|_______________|\n"
    case _ => s"\n\n\n\n\n\n\n\n"
  }

  def lastEnd: Tile = if (openends.isEmpty)
      InvalidTile()
    else openends.last

  def noMove: String = graphics(lastEnd.a) + graphics(lastEnd.a) + graphics(lastEnd.b)

  def emptyScreen: String = graphics(7) + graphics(7)

  def passMove(currentPlayer: Player): String = if (!boneyard.isEmpty)
      s"dominogame:~ Boneyard still not empty and you can't pass a move!\n" +
      s"dominogame:~ A new tile will be added to your pile instead!\n"      +
      drawTile(currentPlayer)
    else  noMove

  def newLast(newEnd: Tile): Tile = openends.moveToEnd(newEnd)

  def openEndDisplay(validatedFirstOpenEnd: Validated[String, Tile]): String = validatedFirstOpenEnd match {
    case Invalid(e) => graphics(lastEnd.a) + graphics(lastEnd.b) + graphics(7) + "dominogame:~ " + e
    case Valid(t: Tile) => graphics(newLast(t).a) + graphics(lastEnd.b) + graphics(7) +
      s"dominogame:~ There are ${openends.length - 1} more open ends to pick from.\n"
  }

  def showNextOpenEnd: String = lastEnd match {
    case InvalidTile(7) => emptyScreen + s"dominogame:~ There are still no open ends!\n"
    case _ => openEndDisplay(openends.getAt(0))
  }

  def nextOpenEndMenu(currentPlayer: Player): IO[Unit] =  putStrLn(showNextOpenEnd  +
                                                          playerInfo(currentPlayer) +
                                                          s"dominogame:~  Again?([-y] - Yes, [-n] - No):")

  def pickNextOrNot(currentPlayer: Player): IO[String] = for {
    _ <- nextOpenEndMenu(currentPlayer)
    input <- getStrLn
  }yield input

  def nextOpenEnd(currentPlayer: Player): String =
    pickNextOrNot(currentPlayer).unsafeRun() match {
      case "-n" =>  identifyCommand(currentPlayer, "")
      case _ => nextOpenEnd(currentPlayer)
    }

  def drawTile(currentPlayer: Player): String =
    currentPlayer.drawTileFromBN(boneyard) match {
      case Some(e) => "dominogame:~ " + e
      case None => noMove
    }

  def addOpenEnd(t: Tile): ArrayBuffer[Tile] = if (openends.isEmpty) {
      if(t.a == t.b){
        openends.add(t)
        openends.add(t)
        openends.add(t)
        openends.add(t)
      } else {
        openends.add(Tile(t.b, t.a))
        openends.add(t)
      }
    } else if(t.a == t.b) {
      openends.add(t)
      openends.add(t)
      openends.add(t)
    } else openends.add(t)

  def firstMove(t: Tile): String = {
    addOpenEnd(t)
    graphics(t.a) + graphics(t.b)
  }

  def wrongMove(currentPlayer: Player): String =
    pickTile(currentPlayer, s"dominogame:~ Tile ends dismatch! Try again!\n")

  def normalMove(t: Tile): String = {
    val last: String = graphics(lastEnd.b)
    if (t.a == lastEnd.b) {
      openends.use(lastEnd)
      addOpenEnd(t)
      last + graphics(t.a) + graphics(t.b)
    } else {
      openends.use(lastEnd)
      addOpenEnd(Tile(t.b, t.a))
      last + graphics(t.b) + graphics(t.a)
    }
  }

  def analysingMove(t: Tile, currentPlayer: Player): String =
    if (openends.isEmpty) firstMove(t)
    else if(t.a != lastEnd.b && t.b != lastEnd.b) wrongMove(currentPlayer)
    else normalMove(t)

  def checkReceivedIndex(validatedNum: Validated[String, Tile], currentPlayer: Player): String =
    validatedNum match {
      case Invalid(e) => pickTile(currentPlayer, "dominogame:~ " + e + "\n")
      case Valid(t) => analysingMove(t, currentPlayer)
    }

  def pickTile(currentPlayer: Player, message: String): String =
    toInt(readCommand(message).unsafeRun()) match {
      case None => pickTile(currentPlayer, "")
      case Some(num) => checkReceivedIndex(currentPlayer.drawTileFromPile(num), currentPlayer)
    }

  def identifyCommand(currentPlayer: Player, message: String):  String =
    readCommand(message).unsafeRun() match {
      case "-pm" => passMove(currentPlayer)
      case "-ne" => nextOpenEnd(currentPlayer)
      case "-dt" => drawTile(currentPlayer)
      case "-p" => pickTile(currentPlayer, "")
      case "-q" => ""
      case _ => identifyCommand(currentPlayer, message)
    }

  def readCommand(message: String): IO[String] = for {
    _ <- putStrLn(message + s"dominogame:~  Pick:")
    input <- getStrLn
  }yield input

  def zeroOpenends(): IO[Unit] = putStrLn(emptyScreen)

  def playerInfo(currentPlayer: Player): String = currentPlayer.announcePlayer() + currentPlayer.printDeck() + "\n"

  def pickMoveLoop(currentPlayer: Player, message: String): Boolean =
    identifyCommand(currentPlayer, message) match {
      case "" => true
      case visualisation: String =>
        putStrLn(visualisation).unsafeRun()
        Thread.sleep(1000)
        putStrLn(graphics(7)).unsafeRun()
        false
    }

  def playerScheduler( quit: Boolean, currentPlayer: Int): IO[Unit] =
    if (player1.pile.length > 0 && player2.pile.length > 0 && !quit) {
      currentPlayer match {
        case 1 => playerScheduler(pickMoveLoop(player1, playerInfo(player1)), 2)
        case 2 => playerScheduler(pickMoveLoop(player2, playerInfo(player2)), 1)
      }
    } else  putStrLn(s"dominogame:~ Calculating results...\n\n")

  def isItHighScore(currentPlayer: Player): IO[Unit] = if(!boneyard.isEmpty) {
      highScoreSystem.saveHighScore(currentPlayer.name, boneyard.length)
      putStrLn(s"dominogame:~ You have reached a HIGH SCORE OF ${boneyard.length}!!!\n")
    } else putStrLn(s"dominogame:~ A victory, indeed... but not a HIGH one!!!\n")

  def showHighScores(): IO[Unit] = highScoreSystem.printHighScore()

  def announceWinner(winner: Player): IO[Unit] = {
    isItHighScore(winner).unsafeRun()
    Thread.sleep(500)
    putStrLn(s"dominogame:~ ${winner.name} IS THE WINNER")
  }

  def checkForWinner: IO[Unit] =
    if (player1.pile.length == 0) {
      announceWinner(player1)
    }else if (player2.pile.length == 0) {
      announceWinner(player2)
    }else putStrLn(s"dominogame:~ No winner this time...")


  def gameloop(): IO[Unit] = {
    zeroOpenends().unsafeRun()
    val start = false
    playerScheduler(start, 1).unsafeRun()
    Thread.sleep(1000)
    checkForWinner.unsafeRun()
    Thread.sleep(500)
    putStrLn(s"\ndominogame:~ GOOD GAME, BYE!" + graphics(7))
  }
}

object Game {
  def instructions(): IO[Unit] = putStrLn(  s"////////////////////////////////////////////////////////////////////////////////" +
                                                s"\n////////////////////////////////   Welcome to:   ///////////////////////////////" +
                                                s"\n////////////////////   [-S-Y-S-A-D-M-I-N-] DRAW DOMINOES   /////////////////////" +
                                                s"\n////////////////////////////////////////////////////////////////////////////////" +
                                                s"\n(*) Instructions:\n> [ -p ] - command for picking a tile to make your"            +
                                                s"\nmove with and you'll be asked to pick it's subsequent number in your pile"        +
                                                s"\n> [ -ne ] - command for displaying another open end"                              +
                                                s"\n> [ -dp ] - command for drawing another tile from"                                +
                                                s"\n the boneyard"                                                                    +
                                                s"\n> [ -pm ] - command for passing a move"                                           +
                                                s"\n> [ -q ] - command for quiting the game\n\n")


  def createBoneyard(array: ArrayBuffer[Tile], count: Int = 0, i: Int = 0, j: Int = 0): ArrayBuffer[Tile] =
    if (count < 28){
      if (i == j) {
        createBoneyard(array += Tile(i, j), count + 1, 0, j + 1)
      }else{
        createBoneyard(array += Tile(i, j), count + 1, i+1, j)
      }
    } else array

  def readPlayerName(currentPlayer: Int): IO[String] = for {
    _ <- putStrLn(s"dominogame:~ Name of Player $currentPlayer:")
    input <- getStrLn
  }yield input

  def apply(): Game = {
    instructions().unsafeRun()
    val boneyard: Deck = Deck(createBoneyard(ArrayBuffer()))
    new Game(Player(readPlayerName(1).unsafeRun(), boneyard),
      Player(readPlayerName(2).unsafeRun(), boneyard), boneyard)
  }
}
