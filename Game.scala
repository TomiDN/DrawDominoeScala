package drawdominoes

import scala.io.StdIn

class Game(val player1: Player,
           val player2: Player,
           val boneyard: Deck[Tile],
           val openends: Deck[Tile] = Deck[Tile](Nil, Nil),
           val lastends: Deck[Tile] = Deck[Tile](Nil, Nil)) {

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

  def lastEnd: Tile = lastends.last

  def passMove: String = graphics(lastEnd.b)

  def newLast(newLast: Tile): Tile = this.lastends.add(newLast).last

  def showNextOpenEnd: String = openends.indexOf(lastEnd) match {

    case -1 => graphics(lastEnd.b) + graphics(7) + s"This is the only open end"

    case n => openends.getAt(n) match {

      case Invalid(a) => graphics(lastEnd.b) + graphics(7) + a

      case Valid(t: Tile) => graphics(newLast(t).b) + graphics(7) + s"Open ends: ${n+1} (out of $openends.length)"

    }

  }

  def nextOpenEndMenu(currentPlayer: Player): Unit = println(showNextOpenEnd           +
    currentPlayer.printDeck() +
    s"dominogame:~  Again?([-y] - Yes, [-n] - No):")

  def nextOpenEnd(currentPlayer: Player): String = {

    nextOpenEndMenu(currentPlayer)

    StdIn.readLine match {

      case "-n" => graphics(lastEnd.b)

      case _ => nextOpenEnd(currentPlayer)

    }

  }

  def wasTileDrawn(currentPlayer: Player): Validated[String, Deck[Tile]] = currentPlayer.drawTileFromBN(boneyard)

  def drawTile(currentPlayer: Player): String = wasTileDrawn(currentPlayer) match {

    case Invalid(a) => a

    case Valid(_) => graphics(lastEnd.b)

  }

  def pickTile(currentPlayer: Player): String = toInt(readCommand) match {

    case None => pickTile(currentPlayer)

    case Some(num) => currentPlayer.drawTileFromPile(num) match {

      case Invalid(_) => pickTile(currentPlayer)

      case Valid(t) => if (openends.isEmpty) {

        openends.add(t)

        graphics(t.b)

      }else if(t.a != lastEnd.b && t.b != lastEnd.b) {

        pickTile(currentPlayer)

      }else if (t.a == lastEnd.b) {

        openends.use(lastEnd)

        openends.add(t)

        graphics(t.a) + graphics(t.b)

      }else {

        openends.use(lastEnd)

        openends.add(Tile (t.b, t.a) )

        graphics(t.b)+graphics(t.a)

      }

    }

  }

  def identifyCommand(currentPlayer: Player):  String = readCommand match {

    case "-pm" => passMove

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


  def zeroOpenends(): Unit = println(graphics(7) + graphics(7))

  def playerInfo(currentPlayer: Player): Unit = println(currentPlayer.announcePlayer() + currentPlayer.printDeck())

  def pickMoveLoop(currentPlayer: Player): Boolean = identifyCommand(currentPlayer) match {

    case "" => false

    case visualisation: String =>

      println(visualisation)

      Thread.sleep(1000)

      println(graphics(7))

      pickMoveLoop(currentPlayer)

  }

  def playerScheduler( quit: Boolean, currentPlayer: Int): Unit = if (player1.pile.length > 0 && player2.pile.length > 0 && !quit) {

    currentPlayer match {

      case 1 => playerScheduler(pickMoveLoop(player1), 2)

      case 2 => playerScheduler(pickMoveLoop(player2), 1)

    }

  } else  println(s"dominogame:~ Calculating results...")


  def gameloop(): Unit = {

    zeroOpenends()

    val start = false

    playerScheduler(start, 1)

    if (player1.pile.length == 0) {

      println(s"dominogame:~ ${player1.name} IS THE WINNER")

    }else if (player2.pile.length == 0) {

      println(s"dominogame:~ ${player2.name} IS THE WINNER")

    }

    println(s"\ndominogame:~ GOOD GAME, BYE!" + graphics(7))

  }

}

object Game {

  def createBoneyard(vec: List[Tile], count: Int = 0, i: Int = 0, j: Int = 0): List[Tile] =

    if (count < 28){

      if (i == j) {

        createBoneyard(vec :+ Tile(i, j), count + 1, i, j + 1)

      }else{

        createBoneyard(vec :+ Tile(i, j), count + 1, i+1, j)

      }

    } else {

      vec :+ Tile(i, j)

    }

  def instructions(): Unit = println(s"////////////////////////////////////////////////////////////////////////////////" +
    s"\n////////////////////////////////   Welcome to:   ///////////////////////////////" +
    s"\n////////////////////   [-S-Y-S-A-D-M-I-N-] DRAW DOMINOES   /////////////////////" +
    s"\n////////////////////////////////////////////////////////////////////////////////" +
    s"\n(*) Instructions:\n> [ -p ] - command for picking a tile to make your" +
    s"\nmove with and you'll be asked to pick it's subsequent number in your pile" +
    s"\n> [ -ne ] - command for displaying another open end" +
    s"\n> [ -dp ] - command for drawing another tile from" +
    s"\n the boneyard" +
    s"\n> [ -pm ] - command for passing a move" +
    s"\n> [ -q ] - command for quiting the game\n\n")

  def readPlayerName(currentPlayer: Int): String = {

    println(s"dominogame:~ Name of Player $currentPlayer:")

    StdIn.readLine

  }

  def apply(): Game = {

    val boneyard: Deck[Tile] = Deck[Tile](createBoneyard(Nil), Nil)

    instructions()

    new Game(Player(readPlayerName(1), boneyard), Player(readPlayerName(2), boneyard), boneyard)

  }

}
