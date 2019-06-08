package drawdominoesgame

import java.io._

import scala.io.Source


class HighScoring(filename: String, printmessage: String, emptyfilemessage: String) {

  def orderingByScore(line: String): Int = line.drop(line.indexOfSlice(" by")).toInt


  def sortHighScores(savedHighScores: Vector[String]): Vector[String] = {

    val savedHighScores: Vector[String] = Source.fromFile(filename).getLines.toVector

    savedHighScores.sortBy(orderingByScore)

  }


  def writeSortedLines(sortedLines: Vector[String], writer: PrintWriter): Unit =

    if(sortedLines.nonEmpty) {

      writer.write(sortedLines.head)

      writeSortedLines(sortedLines.tail, writer)

    } else writer.close()


  def saveHighScore(player: String, score: Int): Unit =

    writeSortedLines(
      sortHighScores(Source.fromFile(filename).getLines.toVector :+ (score + " by " + player)),
      new PrintWriter(new File(filename))
    )


  def printHighScore(): Unit =

    println(printmessage)

    if(Source.fromFile(filename).isEmpty){

      println(emptyfilemessage)

    } else {

      for (line <- Source.fromFile(filename).getLines) {

        println(line)

      }

    }

}

object HighScoring {

  def apply(filename: String, printmessage: String, emptyfilemessage: String): HighScoring =

    new HighScoring(filename, printmessage, emptyfilemessage)

}
