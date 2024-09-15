package y2023

import scala.io.Source

object Day14 extends App {

  val raw =
    Source.fromResource("2023/14.data").getLines().toArray.map(_.toCharArray)

  val finalGrid = spin(raw, 0, 1000000000, Map.empty)

  println(computeLoad(finalGrid))
  def computeLoad(g: Array[Array[Char]]) = {
    g.reverse.zipWithIndex.map { case (row, i) =>
      row.count(_ == 'O') * (i + 1)
    }.sum
  }

  def spin(
      cur: Array[Array[Char]],
      times: Int,
      max: Int,
      acc: Map[String, Int]
  ): Array[Array[Char]] = {
    if (times != max) {
      spin(cur)
      val newTimes = times + 1
      val str = cur.map(new String(_)).mkString("\n")

      acc.get(str) match {
        case Some(i) => {
          val remaining = (max - (newTimes)) % (newTimes - i)
          (0 until remaining).foreach(_ => spin(cur))
          cur
        }
        case None => spin(cur, newTimes, max, acc + (str -> newTimes))
      }
    } else {
      cur
    }

  }

  def spin(in: Array[Array[Char]]) = {
    moveNorth(in)
    moveWest(in)
    moveSouth(in)
    moveEast(in)
  }
  def moveNorth(data: Array[Array[Char]]) = {
    data.indices.filter(_ != 0).foreach { row =>
      data(row).indices.foreach { col =>
        if (data(row)(col) == 'O') {
          val resting = findRestingSpot(data, row, col, _ - 1, identity)
          if (resting != (row, col)) {
            data(resting._1)(resting._2) = 'O'
            data(row)(col) = '.'
          }
        }
      }
    }
  }

  def moveSouth(data: Array[Array[Char]]) = {
    data.indices.filter(_ != data.length - 1).reverse.foreach { row =>
      data(row).indices.foreach { col =>
        if (data(row)(col) == 'O') {
          val resting = findRestingSpot(data, row, col, _ + 1, identity)
          if (resting != (row, col)) {
            data(resting._1)(resting._2) = 'O'
            data(row)(col) = '.'
          }
        }
      }
    }
  }

  def moveEast(data: Array[Array[Char]]) = {
    data.head.indices.filter(_ != data.head.length - 1).reverse.foreach { col =>
      data.indices.foreach { row =>
        if (data(row)(col) == 'O') {
          val resting = findRestingSpot(data, row, col, identity, _ + 1)
          if (resting != (row, col)) {
            data(resting._1)(resting._2) = 'O'
            data(row)(col) = '.'
          }
        }
      }
    }
  }

  def moveWest(data: Array[Array[Char]]) = {
    data.head.indices.filter(_ != 0).foreach { col =>
      data.indices.foreach { row =>
        if (data(row)(col) == 'O') {
          val resting = findRestingSpot(data, row, col, identity, _ - 1)
          if (resting != (row, col)) {
            data(resting._1)(resting._2) = 'O'
            data(row)(col) = '.'
          }
        }
      }
    }
  }

  def findRestingSpot(
      grid: Array[Array[Char]],
      previousRow: Int,
      previousCol: Int,
      nextRow: Int => Int,
      nextCol: Int => Int
  ): (Int, Int) = {
    val nextR = nextRow(previousRow)
    val nextC = nextCol(previousCol)
    if (
      nextR < 0 || nextC < 0 || nextR == grid.length || nextC == grid.head.length
    ) {
      (previousRow, previousCol) // made it to the edge
    } else {
      grid(nextR)(nextC) match {
        case 'O' | '#' => (previousRow, previousCol) // can't move
        case '.'       => findRestingSpot(grid, nextR, nextC, nextRow, nextCol)
      }
    }
  }
}
