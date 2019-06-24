package drawdominoesgame

import java.io._
import scala.io.Source
import drawdominoesgame.io.Console._
import drawdominoesgame.io.IO


class HighScoring(hsFile: File, filename: String, printmessage: String, emptyfilemessage: String) {
  val closing: String = s"---------------------------------------------------------------------------------"

  def orderingByScore(line: String): Int = line.drop(line.indexOfSlice(" by")).toInt

  def sortHighScores(savedHighScores: Vector[String]): Vector[String] = savedHighScores.sortBy(orderingByScore)

  def writeSortedLines(sortedLines: Vector[String], writer: PrintWriter): Unit =
    if(sortedLines.nonEmpty) {
      writer.write(sortedLines.head)
      writeSortedLines(sortedLines.tail, writer)
    } else writer.close()

  def saveHighScore(player: String, score: Int): Unit =
    writeSortedLines(
      sortHighScores(Source.fromFile(filename).getLines.toVector :+ (score + " by " + player)),
      new PrintWriter(hsFile)
    )

  def initiateFile(writer: PrintWriter): Unit = writer.write("")

  def noHighScores: Boolean = Source.fromFile(filename).getLines().toVector.isEmpty

  def fileExists: Boolean =
    if(!hsFile.exists()) {
      initiateFile(new PrintWriter(hsFile))
      false
    } else true

  def printLines(lines: Vector[String], res: String = ""): String =
    if (lines.isEmpty) res
    else printLines(lines.tail, res + lines.head + "\n")

  def printResult: IO[Unit] =
    if (fileExists && noHighScores) putStrLn(emptyfilemessage)
    else putStrLn(printLines(Source.fromFile(filename).getLines.toVector))

  def printHighScore: IO[Unit] = for {
    _ <- putStrLn(printmessage)
    _ <- printResult
    _ <- putStrLn(closing)
  }yield ()
}

object HighScoring {
  def apply(filename: String, printmessage: String, emptyfilemessage: String): HighScoring =
    new HighScoring(new File(filename), filename, printmessage, emptyfilemessage)
}
