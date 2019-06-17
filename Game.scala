package drawdominoesgame

import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO


class Game(val currentPlayer: Player,
           val otherPlayer: Player,
           val boneyard: Deck,
           val openends: Deck) {
  def highScoreSystem: HighScoring = HighScoring("DDHighScores.txt",
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

  def noMove: String = graphics(lastEnd.a) + graphics(lastEnd.a) + graphics(lastEnd.b)

  def emptyScreen: String = graphics(7) + graphics(7)

  def unsuccessfulpassMove: String =
      s"dominogame:~ Boneyard still not empty and you can't pass a move!\n" +
      s"dominogame:~ A new tile will be added to your pile instead!\n"

  def openEndDisplay: String =
      graphics(lastEnd.a) + graphics(lastEnd.b) + graphics(7) +
        s"dominogame:~ There are ${openends.length - 1} more open ends to pick from.\n"

  def firstMove(t: Tile): String = graphics(t.a) + graphics(t.b) + graphics(7)

  def wrongMove(): String = s"dominogame:~ Tile ends dismatch! Try again!\n"

  def normalMove(t: Tile): String = {
    val last: String = graphics(lastEnd.b)
    if (t.a == lastEnd.b) last + graphics(t.a) + graphics(t.b) + graphics(7)
    else last + graphics(t.b) + graphics(t.a) + graphics(7)
  }

  def playerInfo: String = currentPlayer.announcePlayer() + currentPlayer.printDeck() + "\n"

  def isItHighScore(currentPlayer: Player): IO[Unit] = if(!boneyard.isEmpty) {
      highScoreSystem.saveHighScore(currentPlayer.name, boneyard.length)
      putStrLn(s"dominogame:~ You have reached a HIGH SCORE OF ${boneyard.length}!!!\n")
    } else putStrLn(s"dominogame:~ A victory, indeed... but not a HIGH one!!!\n")

  def showHighScores(): IO[Unit] = highScoreSystem.printHighScore()

  def announceWinner(winner: Player): IO[Unit] = for {
    _ <- putStrLn(s"dominogame:~ ${winner.name} IS THE WINNER")
    _ <- isItHighScore(winner)
  }yield ()

  def checkForWinner: IO[Unit] =
    if (currentPlayer.pile.length == 0) {
      announceWinner(currentPlayer)
    }else if (otherPlayer.pile.length == 0) {
      announceWinner(otherPlayer)
    }else putStrLn(s"dominogame:~ No winner this time...")

  def endGame: IO[Unit] = for {
    _ <- putStrLn(s"dominogame:~ Calculating results...\n\n")
    _ <- checkForWinner
    _ <- putStrLn(s"\ndominogame:~ GOOD GAME, BYE!" + graphics(7))
  }yield ()
}

case class newGame(player1: String, player2: String, initialBoneyard: Deck, gamerUI: GamerUI) extends
  Game( Player(player1, gamerUI.fillDeck(initialBoneyard)),
        Player(player2, gamerUI.fillDeck(initialBoneyard)),
        initialBoneyard,
        Deck(List()))

case class firstMoveGame(gameEntity: Game, gamerUI: GamerUI, chosen: Tile) extends
  Game( gameEntity.otherPlayer,
    Player(gameEntity.currentPlayer.name, gameEntity.currentPlayer.pile.use(chosen)),
    gameEntity.boneyard,
    gamerUI.addOpenEnd(chosen, gameEntity.openends))

case class gameWithMove(gameEntity: Game, gamerUI: GamerUI, chosen: Tile) extends
  Game( gameEntity.otherPlayer,
        Player(gameEntity.currentPlayer.name, gameEntity.currentPlayer.pile.use(chosen)),
        gameEntity.boneyard,
        gamerUI.addOpenEnd(chosen, gameEntity.openends.use(gameEntity.lastEnd)))

case class gameWithDrawnTile(gameEntity: Game, piece: Tile) extends
  Game( gameEntity.otherPlayer,
        Player(gameEntity.currentPlayer.name, gameEntity.currentPlayer.pile.add(piece)),
        gameEntity.boneyard,
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
  def apply(currentPlayer: Player, otherPlayer: Player, boneyard: Deck, openends: Deck): Game =
    new Game(currentPlayer, otherPlayer, boneyard, openends)
}
