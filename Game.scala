package drawdominoesgame

import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO


class Game(val currentPlayer: Player,
           val otherPlayer: Player,
           val boneyard: Deck,
           val openends: Deck) {
  val emptySpace: Int = 7

  def highScoreSystem: HighScoring =
    HighScoring("DDHighScores.txt",
                s"[-S-Y-S-A-D-M-I-N-] DRAW DOMINOES HIGH SCORES\n\n" +
                s"format is:\n"                                      +
                s"[player name] - [count of tiles (higher than 0) that were left in the boneyard]:\n\n",
                s"Still no high scores... Noobs...")

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

  def lastEnd: Tile = openends.last

  val emptyScreen: String = graphics(emptySpace) + graphics(emptySpace)

  val wrongMove: String = Game.commandLine + s"Tile ends dismatch! Try again!\n"

  val unsuccessfulpassMove: String =
        Game.commandLine + s"Boneyard still not empty and you can't pass a move!\n" +
        Game.commandLine + s"A new tile will be added to your pile instead!\n"

  def noMove: String = graphics(lastEnd.a) + graphics(lastEnd.a) + graphics(lastEnd.b)

  def openEndDisplay: String =
        graphics(lastEnd.a) + graphics(lastEnd.b) + graphics(emptySpace) +
        Game.commandLine + s"There are ${openends.length - 1} more open ends to pick from.\n"

  def firstMove(t: Tile): String = graphics(t.a) + graphics(t.b) + graphics(emptySpace)

  def normalMove(t: Tile): String = {
    val last: String = graphics(lastEnd.b)
    if (t.a == lastEnd.b) last + graphics(t.a) + graphics(t.b) + graphics(emptySpace)
    else last + graphics(t.b) + graphics(t.a) + graphics(emptySpace)
  }

  def playerInfo: String = currentPlayer.announcePlayer() + currentPlayer.printDeck() + "\n"

  def isItHighScore(currentPlayer: Player): IO[Unit] =
    if(!boneyard.isEmpty) {
      highScoreSystem.saveHighScore(currentPlayer.name, boneyard.length)
      putStrLn(Game.commandLine + s"You have reached a HIGH SCORE OF ${boneyard.length}!!!\n")
    } else putStrLn(Game.commandLine + s"A victory, indeed... but not a HIGH one!!!\n")

  def showHighScores(): IO[Unit] = highScoreSystem.printHighScore()

  def announceWinner(winner: Player): IO[Unit] = for {
    _ <- putStrLn(Game.commandLine + s"${winner.name} IS THE WINNER")
    _ <- isItHighScore(winner)
  }yield ()

  def checkForWinner: IO[Unit] =
    if (currentPlayer.pile.length == 0) {
      announceWinner(currentPlayer)
    }else if (otherPlayer.pile.length == 0) {
      announceWinner(otherPlayer)
    }else putStrLn(Game.commandLine + s"No winner this time...")

  def endGame: IO[Unit] = for {
    _ <- putStrLn(Game.commandLine + s"Calculating results...\n\n")
    _ <- checkForWinner
    _ <- putStrLn(Game.commandLine + s"GOOD GAME, BYE!" + graphics(emptySpace))
  }yield ()
}

case class firstMoveGame(gameEntity: Game, chosen: Tile) extends
  Game( gameEntity.otherPlayer,
        Player(gameEntity.currentPlayer.name, gameEntity.currentPlayer.pile.use(chosen)),
        gameEntity.boneyard,
        Game.addOpenEnd(chosen, gameEntity.openends))

case class gameWithMove(gameEntity: Game, chosen: Tile) extends
  Game( gameEntity.otherPlayer,
        Player(gameEntity.currentPlayer.name, gameEntity.currentPlayer.pile.use(chosen)),
        gameEntity.boneyard,
        Game.addOpenEnd(chosen, gameEntity.openends.use(gameEntity.lastEnd)))

case class gameWithDrawnTile(gameEntity: Game, piece: Tile) extends
  Game( gameEntity.otherPlayer,
        Player(gameEntity.currentPlayer.name, gameEntity.currentPlayer.pile.add(piece)),
        gameEntity.boneyard.use(piece),
        gameEntity.openends)

case class gameWithNewOpenEnd(gameEntity: Game, piece: Tile) extends
  Game( gameEntity.currentPlayer,
        gameEntity.otherPlayer,
        gameEntity.boneyard,
        gameEntity.openends.moveToEnd(piece))

case class simpleSwitchGame(gameEntity: Game) extends
  Game( gameEntity.otherPlayer,
        gameEntity.currentPlayer,
        gameEntity.boneyard,
        gameEntity.openends)

object Game {
  val commandLine: String = s"dominogame:~  "

  val pickLine: String = commandLine + s" Pick:"

  val nextMove: String = "-ne"

  val drawTile: String = "-dt"

  val passMove: String = "-pm"

  val quitGame: String = "-q"

  val instructions: String =
                    s"////////////////////////////////////////////////////////////////////////////////" +
                  s"\n////////////////////////////////   Welcome to:   ///////////////////////////////" +
                  s"\n////////////////////   [-S-Y-S-A-D-M-I-N-] DRAW DOMINOES   /////////////////////" +
                  s"\n////////////////////////////////////////////////////////////////////////////////" +
                  s"\n(*) Instructions:"                                                                +
                  s"\n> [ {a number} ] - command for picking the subsequent number of a tile"           +
                  s"\n                   in your pile to make a move with"                              +
                  s"\n> [    $nextMove     ] - command for displaying another open end"                 +
                  s"\n> [    $drawTile     ] - command for drawing another tile from the boneyard"      +
                  s"\n> [    $passMove     ] - command for passing a move"                              +
                  s"\n> [     $quitGame     ] - command for quiting the game\n\n"

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

  def startNewGame(name1: String, name2: String): Game = {
    val initialFirstState: Deck = Game.initialDeck
    val player1: Player = Player(name1, initialFirstState)
    val initialSecondState: Deck = Game.initialDeck.diff(player1.pile)
    val player2: Player = Player(name2, initialSecondState)
    Game(player1, player2, initialSecondState.diff(player2.pile))
  }

  def apply(currentPlayer: Player, otherPlayer: Player, boneyard: Deck, openends: Deck = Deck(List())): Game =
    new Game(currentPlayer, otherPlayer, boneyard, openends)
}
