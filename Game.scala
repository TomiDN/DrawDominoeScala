package drawdominoesgame

import scala.io.StdIn

import scala.collection.mutable.ArrayBuffer


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

    case 6 =>  s"_______________" +
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


  def showNextOpenEnd: String = lastEnd match {

    case InvalidTile(7) => emptyScreen + s"dominogame:~ There are still no open ends!\n"

    case _ => openends.getAt(0) match {

      case Invalid(e) => graphics(lastEnd.a) + graphics(lastEnd.b) + graphics(7) + "dominogame:~ " + e

      case Valid(t: Tile) => graphics(newLast(t).a) + graphics(lastEnd.b) + graphics(7) +
        s"dominogame:~ There are ${openends.length - 1} more open ends to pick from.\n"

    }

  }


  def nextOpenEndMenu(currentPlayer: Player): Unit = println(showNextOpenEnd           +
                                                             playerInfo(currentPlayer) +
                                                             s"dominogame:~  Again?([-y] - Yes, [-n] - No):")


  def nextOpenEnd(currentPlayer: Player): String = {

    nextOpenEndMenu(currentPlayer)

    StdIn.readLine match {

      case "-n" =>  s"dominogame:~  Pick:\n" + identifyCommand(currentPlayer)

      case _ => nextOpenEnd(currentPlayer)

    }

  }


  def wasTileDrawn(currentPlayer: Player): Validated[String, ArrayBuffer[Tile]] = currentPlayer.drawTileFromBN(boneyard)


  def drawTile(currentPlayer: Player): String = wasTileDrawn(currentPlayer) match {

    case Invalid(e) => "dominogame:~ " + e

    case Valid(_) => noMove

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


  def pickTile(currentPlayer: Player): String = toInt(readCommand) match {

    case None => pickTile(currentPlayer)

    case Some(num) => currentPlayer.drawTileFromPile(num) match {

      case Invalid(e) =>

        println("dominogame:~ " + e)

        pickTile(currentPlayer)

      case Valid(t) => if (openends.isEmpty) {

        addOpenEnd(t)

        graphics(t.a) + graphics(t.b)

      }else if(t.a != lastEnd.b && t.b != lastEnd.b) {

        println(s"dominogame:~ Tile ends dismatch! Try again!")

        pickTile(currentPlayer)

      }else {

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

    }

  }


  def identifyCommand(currentPlayer: Player):  String = readCommand match {

    case "-pm" => passMove(currentPlayer)

    case "-ne" => nextOpenEnd(currentPlayer)

    case "-dt" => drawTile(currentPlayer)

    case "-p" => pickTile(currentPlayer)

    case "-q" => ""

    case _ => identifyCommand(currentPlayer)

  }


  def readCommand: String = {

    println(s"dominogame:~  Pick:")

    StdIn.readLine

  }


  def zeroOpenends(): Unit = println(emptyScreen)


  def playerInfo(currentPlayer: Player): String = currentPlayer.announcePlayer() + currentPlayer.printDeck()


  def pickMoveLoop(currentPlayer: Player): Boolean = identifyCommand(currentPlayer) match {

    case "" => true

    case visualisation: String =>

      println(visualisation)

      Thread.sleep(1000)

      println(graphics(7))

      false

  }


  def playerScheduler( quit: Boolean, currentPlayer: Int): Unit = if (player1.pile.length > 0 && player2.pile.length > 0 && !quit) {

    currentPlayer match {

      case 1 =>

        println(playerInfo(player1))

        playerScheduler(pickMoveLoop(player1), 2)

      case 2 =>

        println(playerInfo(player2))

        playerScheduler(pickMoveLoop(player2), 1)

    }

  } else  println(s"dominogame:~ Calculating results...\n\n")


  def isItHighScore(currentPlayer: Player): Unit = if(!boneyard.isEmpty) {

    highScoreSystem.saveHighScore(currentPlayer.name, boneyard.length)

    println(s"dominogame:~ You have reached a HIGH SCORE OF ${boneyard.length}!!!\n")

  } else println(s"dominogame:~ A victory, indeed... but not a HIGH one!!!\n")


  def showHighScores(): Unit = highScoreSystem.printHighScore()


  def gameloop(): Unit = {

    zeroOpenends()

    val start = false

    playerScheduler(start, 1)

    Thread.sleep(1000)

    if (player1.pile.length == 0) {

      isItHighScore(player1)

      Thread.sleep(500)

      println(s"dominogame:~ ${player1.name} IS THE WINNER")

    }else if (player2.pile.length == 0) {

      isItHighScore(player2)

      Thread.sleep(500)

      println(s"dominogame:~ ${player2.name} IS THE WINNER")

    }

    Thread.sleep(500)

    println(s"\ndominogame:~ GOOD GAME, BYE!" + graphics(7))

  }

}

object Game {

  def createBoneyard(array: ArrayBuffer[Tile], count: Int = 0, i: Int = 0, j: Int = 0): ArrayBuffer[Tile] =

    if (count < 28){

      if (i == j) {

        createBoneyard(array += Tile(i, j), count + 1, 0, j + 1)

      }else{

        createBoneyard(array += Tile(i, j), count + 1, i+1, j)

      }

    } else array


  def instructions(): Unit = println( s"////////////////////////////////////////////////////////////////////////////////" +
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


  def readPlayerName(currentPlayer: Int): String = {

    println(s"dominogame:~ Name of Player $currentPlayer:")

    StdIn.readLine

  }


  def apply(): Game = {

    val boneyard: Deck = Deck(createBoneyard(ArrayBuffer()))

    instructions()

    new Game(Player(readPlayerName(1), boneyard), Player(readPlayerName(2), boneyard), boneyard)

  }

}

