package y2021

object Day21 extends App {


  //val results = playGame(Player(10, 0, 0),Player(6,0, 0), 0, new DeterministicDiceRoller())

  //  println(results.loser.score * results.timesRolled)


  val all = (for {
    f <- Seq(1, 2, 3)
    s <- Seq(1, 2, 3)
    t <- Seq(1, 2, 3)
  } yield {
    Seq(f, s, t)
  }).map(_.sum)
  //map of 3 roll outcomes, and their counts
  /*val sums: Map[Int, Int] = all.map(_.sum).groupMapReduce(identity)(_ => 1)(_ + _)
  println(sums)*/

  val reF = playGame22(Player(10, 0), Player(6, 0))
  println(reF)


  def playGame22(player1: Player, player2: Player): GameResults2 = {

    val cache = scala.collection.mutable.Map.empty[(Player, Player), GameResults2]

    def doTheThing(p1: Player, p2: Player): GameResults2 = {
      val cached = cache.get((p1, p2))
      cached match {
        case Some(p) => p
        case None => {
          //val p1Outcomes = sums.keys.map(x => (p1.advance(x), x, sums(x))) //27 outcomes b/c 3 rolls of 3 outcomes
          val p1Outcomes = all.map(x => p1.advance(x))
          val (p1Winners, nonP1Winners) = p1Outcomes.partition(_.isWinner)

          val p1ResUpdates = if (p1Winners.nonEmpty) {
            val totalUniverses = p1Winners.size
            GameResults2(player1Wins = totalUniverses, 0)
          } else {
            GameResults2(0, 0)
          }

          if (nonP1Winners.isEmpty) { //p1 wins it all, no need to continue
            cache.put((p1, p2), p1ResUpdates)
            p1ResUpdates
          } else {
            val p2Outcomes = for {
              //(p1nw, _, uniCount) <- nonP1Winners
              p1nw <- nonP1Winners
              //(p2n, _, p2UniCount) <- sums.keys.map(x => (p2.advance(x), x, sums(x)))
              (p2n) <- all.map(x => p2.advance(x))
            } yield {
              //(p1nw, p2n, uniCount * p2UniCount)
              (p1nw, p2n)
            }

            val (p2Winners, nonP2Winners) = p2Outcomes.partition(_._2.isWinner)

            val p2ResUpdates = if (p2Winners.nonEmpty) {
              //val totalUniverses = p2Winners.map(_._3).sum
              val totalUniverses = p2Winners.size
              p1ResUpdates.copy(player2Wins = totalUniverses)
            } else {
              p1ResUpdates
            }

            if (nonP2Winners.isEmpty) { //no more games can ontinue
              cache.put((p1, p2), p2ResUpdates)
              p2ResUpdates
            } else {
              val nextRounds = nonP2Winners.map { case (nextP1, nextP2) =>
                doTheThing(nextP1, nextP2)
                //r.copy(player1Wins = r.player1Wins * count, player2Wins = r.player2Wins * count)
              }
              val updatedRes = p2ResUpdates.combine(nextRounds.reduce(_.combine(_)))
              cache.put((p1, p2), updatedRes)
              updatedRes
            }
          }
        }
      }
    }

    doTheThing(player1, player2)
  }

  case class GameResults2(player1Wins: Long, player2Wins: Long) {


    def combine(other: GameResults2) = this.copy(player1Wins = this.player1Wins + other.player1Wins, player2Wins = this.player2Wins + other.player2Wins)


  }

  def playGame(player1: Player, player2: Player, timesRolled: Int, roller: DiceRoller): GameResults = {

    val player1Rolls = roller.roll(3).sum
    val updatedPlayer1 = player1.advance(player1Rolls)
    if (updatedPlayer1.score >= 1000) {
      GameResults(updatedPlayer1, player2, timesRolled + 3)
    } else {
      val player2Rolls = roller.roll(3).sum
      val updatedPlayer2 = player2.advance(player2Rolls)
      if (updatedPlayer2.score >= 1000) {
        GameResults(updatedPlayer1, updatedPlayer2, timesRolled + 6)
      } else {
        playGame(updatedPlayer1, updatedPlayer2, timesRolled + 6, roller)
      }
    }
  }


  case class Player(position: Int, score: Int) {

    def advance(spaces: Int): Player = {
      val actualMoves = spaces % 10
      val newLocation = position + actualMoves

      val actualLocation = if (newLocation > 10) newLocation % 10 else newLocation
      this.copy(actualLocation, score + actualLocation)
    }

    def isWinner: Boolean = score >= 21

  }

  case class GameResults(winner: Player, loser: Player, timesRolled: Int)


  trait DiceRoller {
    def roll(times: Int): Seq[Int]
  }


  class DeterministicDiceRoller() extends DiceRoller {

    var currentNumber = 0


    override def roll(times: Int): Seq[Int] = {
      (1 to times).map(_ => nextValue)
    }

    private def nextValue: Int = {
      currentNumber += 1
      if (currentNumber > 100) {
        currentNumber = 1
      }
      currentNumber
    }
  }

}
