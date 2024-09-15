package y2015

import scala.io.Source

object Day3 extends App {

  val data = Source.fromResource("2015/3.data").toSeq

  val init = Map((0, 0) -> 2)
  val distribution = roboDistributePresents(init, data, (0, 0), (0, 0), 0)

  println(distribution)

  def distributePresents(
      houses: Map[(Int, Int), Int],
      directions: Seq[Char],
      currentHouse: (Int, Int)
  ): Int = {

    val curPresCount = houses.getOrElse(currentHouse, 0)
    val updatedHouses = houses + (currentHouse -> (curPresCount + 1))

    if (directions.nonEmpty) {
      val newHouse = directions.head match {
        case '>' => (currentHouse._1, currentHouse._2 + 1)
        case '<' => (currentHouse._1, currentHouse._2 - 1)
        case '^' => (currentHouse._1 - 1, currentHouse._2)
        case 'v' => (currentHouse._1 + 1, currentHouse._2)
      }
      distributePresents(updatedHouses, directions.tail, newHouse)
    } else {
      updatedHouses.size
    }
  }

  def roboDistributePresents(
      houses: Map[(Int, Int), Int],
      directions: Seq[Char],
      santaLoc: (Int, Int),
      robotLoc: (Int, Int),
      moveCount: Int
  ): Int = {

    if (directions.isEmpty) {
      houses.size
    } else {

      val whoMoves = if (moveCount % 2 == 0) {
        santaLoc
      } else {
        robotLoc
      }

      val updatedLoc = directions.head match {
        case '>' => (whoMoves._1, whoMoves._2 + 1)
        case '<' => (whoMoves._1, whoMoves._2 - 1)
        case '^' => (whoMoves._1 - 1, whoMoves._2)
        case 'v' => (whoMoves._1 + 1, whoMoves._2)
      }

      val curPresCount = houses.getOrElse(updatedLoc, 0)
      val updatedHouses = houses + (updatedLoc -> (curPresCount + 1))

      if (moveCount % 2 == 0) {
        roboDistributePresents(
          updatedHouses,
          directions.tail,
          updatedLoc,
          robotLoc,
          moveCount + 1
        )
      } else {
        roboDistributePresents(
          updatedHouses,
          directions.tail,
          santaLoc,
          updatedLoc,
          moveCount + 1
        )
      }
    }
  }

}
