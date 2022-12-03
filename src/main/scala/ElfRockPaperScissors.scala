import scala.io.Source

case class playerRoundScores(opponentGuess: Int, myElfGuess: Int)

object ElfRockPaperScissors {
  def openFile(filename: String): String = {
    val dataSource = Source.fromFile(filename)
    val lines: String = dataSource.mkString
    dataSource.close
    lines
  }

  def roundTotaller(inputString: String): Int = {
    val allRounds: Array[String] = inputString.split("\n")
    allRounds.map((round: String) => {
      val opponentGuess= round.charAt(0).toString
      val myElfGuess= round.charAt(2).toString
      generateRoundScores(opponentGuess, myElfGuess)
      }
    ).sum
  }

  def generateRoundScores(opponentGuess: String, myElfGuess: String): Int = {
    val guessScore = rpsScoreLookup(myElfGuess)
    val roundScore = findRoundWinner(opponentGuess, myElfGuess).myElfGuess
    guessScore + roundScore
  }

  def rpsScoreLookup(rockPaperOrScissors: String): Int = {
    val rockPaperScissorsMap = Map("X" -> 1, "Y" -> 2, "Z" -> 3)
    rockPaperScissorsMap.get(rockPaperOrScissors) match {
      case Some(number) => number
      case None => -1
    }
  }

  def findRoundWinner(opponentGuess: String, myElfGuess: String): playerRoundScores = {
    (opponentGuess, myElfGuess) match {
      case ("A", "X") | ("B", "Y") | ("C", "Z") => playerRoundScores(3, 3)
      case ("A", "Y") | ("B", "Z") | ("C", "X") => playerRoundScores(0, 6)
      case ("A", "Z") | ("B", "X") | ("C", "Y") => playerRoundScores(6, 0)
    }
  }
}
