package y2021

import scala.io.Source

object Day25 extends App {
  val data = Source.fromResource("2021/25.data").getLines().toSeq

  val maxRows = data.size - 1
  val maxColumns = data.head.length - 1

  val (r, d, e) =
    data.zipWithIndex
      .foldLeft(
        (Set.empty[(Int, Int)], Set.empty[(Int, Int)], Set.empty[(Int, Int)])
      ) { case ((rc, dc, ec), (row, rIndex)) =>
        row.zipWithIndex.foldLeft((rc, dc, ec)) {
          case ((rc2, dc2, ec2), (ch, cIndex)) =>
            ch match {
              case '>' => (rc2 + (rIndex -> cIndex), dc2, ec2)
              case 'v' => (rc2, dc2 + (rIndex -> cIndex), ec2)
              case '.' => (rc2, dc2, ec2 + (rIndex -> cIndex))
            }
        }
      }

  println(shiftUntilDone(r, d, e, 0))

  def shiftNTimes(
      rightCoords: Set[(Int, Int)],
      downCoords: Set[(Int, Int)],
      emptyCoords: Set[(Int, Int)],
      timesRemaining: Int
  ): (Set[(Int, Int)], Set[(Int, Int)], Set[(Int, Int)]) = {
    println("------------------")
    printGrid(rightCoords, downCoords, emptyCoords)
    if (timesRemaining == 0) {
      (rightCoords, downCoords, emptyCoords)
    } else {
      val (nr, nd, ne) = shiftArray(rightCoords, downCoords, emptyCoords)
      shiftNTimes(nr, nd, ne, timesRemaining - 1)
    }

  }

  def shiftUntilDone(
      rightCoords: Set[(Int, Int)],
      downCoords: Set[(Int, Int)],
      emptyCoords: Set[(Int, Int)],
      timesShifted: Int
  ): Int = {
    val (nr, nd, ne) = shiftArray(rightCoords, downCoords, emptyCoords)
    val newShiftCount = timesShifted + 1
    if (emptyCoords == ne) { // nothing changed
      printGrid(nr, nd, ne)
      newShiftCount
    } else {
      shiftUntilDone(nr, nd, ne, newShiftCount)
    }
  }

  def shiftArray(
      rightCoords: Set[(Int, Int)],
      downCoords: Set[(Int, Int)],
      emptyCoords: Set[(Int, Int)]
  ): (Set[(Int, Int)], Set[(Int, Int)], Set[(Int, Int)]) = {

    val (updatedRight, updatedEmpty) = moveRight(rightCoords, emptyCoords)
    val (updatedDown, finalEmpty) = moveDown(downCoords, updatedEmpty)
    (updatedRight, updatedDown, finalEmpty)
  }

  // need to swap right and empty
  def moveRight(
      rightCoords: Set[(Int, Int)],
      emptyCoords: Set[(Int, Int)]
  ): (Set[(Int, Int)], Set[(Int, Int)]) = {

    move(rightCoords, emptyCoords) { (r, c) =>
      val updatedC = if (c == maxColumns) 0 else c + 1
      (r, updatedC)
    }
  }

  def moveDown(
      downCoords: Set[(Int, Int)],
      emptyCoords: Set[(Int, Int)]
  ): (Set[(Int, Int)], Set[(Int, Int)]) = {
    move(downCoords, emptyCoords) { (r, c) =>
      val updatedR = if (r == maxRows) 0 else r + 1
      (updatedR, c)
    }
  }

  def move(rightCoords: Set[(Int, Int)], emptyCoords: Set[(Int, Int)])(
      f: (Int, Int) => (Int, Int)
  ): (Set[(Int, Int)], Set[(Int, Int)]) = {
    rightCoords.foldLeft((Set.empty[(Int, Int)], Set.from(emptyCoords))) {
      case ((newRight, newEmpty), (r, c)) =>
        val nextMove = f(r, c)
        if (emptyCoords.contains(nextMove)) {
          val updatedEmpty = newEmpty - (nextMove)
          (newRight + nextMove, updatedEmpty + (r -> c))
        } else {
          (newRight + (r -> c), newEmpty)
        }

    }
  }

  def printGrid(
      right: Set[(Int, Int)],
      down: Set[(Int, Int)],
      empty: Set[(Int, Int)]
  ): Unit = {

    for {
      r <- 0 to maxRows
    } {

      val str = (0 to maxColumns).map { c =>
        val coord = (r, c)
        if (right.contains(coord)) {
          '>'
        } else if (down.contains(coord)) {
          'v'
        } else {
          '.'
        }
      }
      println(new String(str.toArray))

    }

  }

}
