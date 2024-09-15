package y2015

import scala.collection.mutable.{Map => MMap}
object Day20 extends App {

  val target = 36000000

  // 3272728 too large
  // 856800 //wrong
  // 776160 too low

  println(findHouse(1))
  println(findHouse2(1, MMap.empty))

  def findHouse(houseNumber: Int): Int = {

    // +, -, /, *, %   4/2   2 r 0  5 % 2   2 ,1
    val amount = (1 to Math.sqrt(houseNumber).toInt).foldLeft(0) {
      case (acc, x) =>
        if (houseNumber % x == 0) {
          val remaining = houseNumber / x
          if (remaining != x) {
            acc + (x * 10) + (remaining * 10)
          } else {
            acc + (x * 10)
          }

        } else {
          acc
        }
    }

    if (amount >= target) {
      houseNumber
    } else {
      findHouse(houseNumber + 1)
    }
  }

  /*
  100

   */
  def findHouse2(houseNumber: Int, used: MMap[Int, Int]): Int = {
//50 - 50/.0 1  51 - 50

    val minPrintingBound = 40000
    val maxPrintingBound = 50000
    if (houseNumber >= minPrintingBound && houseNumber <= maxPrintingBound) {
      println(s"checking house: $houseNumber")
    }
    // 1,2,3,4,5,6,7,8,9,10
    val amount = (1 to Math.sqrt(houseNumber).toInt).foldLeft(0) {
      case (acc, x) =>
        if (houseNumber % x == 0) { // 1 to 10
          val remaining = houseNumber / x // 1,
          if (
            houseNumber >= minPrintingBound && houseNumber <= maxPrintingBound
          ) {
            println(s" -- checking ($x, $remaining) ")
          }

          val firstFactor = if (used.getOrElse(x, 0) < 50) {
            used.put(x, used.getOrElse(x, 0) + 1)
            x * 11
          } else {
            0
          }
          val secondFactor =
            if (remaining != x && used.getOrElse(remaining, 0) < 50) {
              used.put(remaining, used.getOrElse(remaining, 0) + 1)
              remaining * 11
            } else {
              0
            }

          acc + firstFactor + secondFactor

        } else {
          acc
        }
    }
    if (houseNumber >= minPrintingBound && houseNumber <= maxPrintingBound) {

      println("------------")
    }
    if (amount >= target) {
      houseNumber
    } else {
      findHouse2(houseNumber + 1, used)
    }
  }

}
