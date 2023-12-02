package y2021

import scala.io.Source

object Day11 extends App {


  val data = Source.fromResource("2021/11.data").getLines()

  val array = Array.ofDim[Int](10, 10)
  data.zipWithIndex.foreach { case (row, r) =>
    row.zipWithIndex.foreach { case (value, c) =>
      array(r)(c) = value.toString.toInt

    }
  }

  /*val totalFlashes = doTheThing(0, 100)
  println(totalFlashes)*/

  println(findSyncFlash(1))


  def doTheThing(flashes: Int, timesRemaining: Int): Int = {

    if (timesRemaining == 0) {
      flashes
    } else {

      //first increment the grid
      val newFlashes = (for {
        (row, r) <- array.zipWithIndex
        (i, c) <- row.zipWithIndex
      } yield {
        val nextValue = i + 1
        array(r)(c) = nextValue
        if (nextValue > 9) {
          Seq((r, c))
        } else {
          Seq.empty
        }
      }).flatten

      propagateFlashes(newFlashes.toSet)

      val totalFlashes = array.map(_.count(_ == 0)).sum

      doTheThing(flashes + totalFlashes, timesRemaining - 1)
    }
  }


  def findSyncFlash(round: Int): Int = {


    //first increment the grid
    val newFlashes = (for {
      (row, r) <- array.zipWithIndex
      (i, c) <- row.zipWithIndex
    } yield {
      val nextValue = i + 1
      array(r)(c) = nextValue
      if (nextValue > 9) {
        Seq((r, c))
      } else {
        Seq.empty
      }
    }).flatten

    //gives all flashes (values > 9)
    propagateFlashes(newFlashes.toSet)

    val totalFlashes = array.map(_.count(_ == 0)).sum

    if (totalFlashes == 100) {
      round
    } else {
      findSyncFlash(round + 1)
    }
  }


  def propagateFlashes(flashes: Set[(Int, Int)]): Unit = {
    if (flashes.isEmpty) {
      ()
    } else {
      val (r, c) = flashes.head
      array(r)(c) = 0
      val surroundingFlashes = getSurroundingCoords(r, c).foldLeft(Set.empty[(Int, Int)]) { case (acc, (r1, c1)) =>
        val value = array(r1)(c1)
        if (value == 0) {
          acc
        } else {
          val updated = value + 1
          array(r1)(c1) = updated
          if (updated > 9) {
            acc + (r1 -> c1)
          } else {
            acc
          }
        }
      }
      propagateFlashes(flashes.tail ++ surroundingFlashes)
    }
  }

  def getCoord(r: Int, c: Int): Option[(Int, Int)] = {
    if (r < 0 || r == 10 || c < 0 || c == 10) {
      None
    } else {
      Some((r, c))
    }
  }

  def getSurroundingCoords(r: Int, c: Int): Seq[(Int, Int)] = {
    Seq(
      getCoord(r + 1, c), //below
      getCoord(r - 1, c), //above
      getCoord(r, c + 1), //right
      getCoord(r, c - 1), //left
      getCoord(r + 1, c + 1), //diag lower right
      getCoord(r + 1, c - 1), //diag lower left
      getCoord(r - 1, c + 1), //diag upper right
      getCoord(r - 1, c - 1), //diag upper left
    ).flatten
  }


  def printGrid(step: Int): Unit = {

    println(s"steps remaining: $step")
    println("- - - - - - - - - -")
    array.foreach(col => println(col.mkString(" ")))
    println("- - - - - - - - - -")
    println("\n\n")

  }

}
