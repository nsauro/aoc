package y2021

import scala.io.Source

object Day5Part1 extends App {

  val data = Source.fromResource("2021/5.data").getLines()

  val Coords = raw"(\d+),(\d+) -> (\d+),(\d+)".r

  def addLine(
      map: Map[(Int, Int), Int],
      coords: Seq[(Int, Int)]
  ): Map[(Int, Int), Int] = {
    coords.foldLeft(map) { case (acc, coord) =>
      acc.get(coord) match {
        case None       => acc + (coord -> 1)
        case Some(prev) => acc + (coord -> (prev + 1))
      }
    }
  }

  val allPoints = data.foldLeft(Map.empty[(Int, Int), Int]) {
    case (acc, line) =>
      line match {
        case Coords(x1s, y1s, x2s, y2s) =>
          val x1 = x1s.toInt
          val y1 = y1s.toInt
          val x2 = x2s.toInt
          val y2 = y2s.toInt
          if (x1 == x2) {
            val min = Math.min(y1, y2)
            val max = Math.max(y1, y2)
            val line = (min to max).map((x1, _))
            addLine(acc, line)
          } else if (y1 == y2) {
            val min = Math.min(x1, x2)
            val max = Math.max(x1, x2)
            val line = (min to max).map((_, y2))
            addLine(acc, line)
          } else {
            val xDir = x2.compare(x1) // 1 means x is greater
            val yDir = y2.compare(y1) // 1 means y is greater
            val lineX = Range.inclusive(x1, x2, xDir)
            val lineY = Range.inclusive(y1, y2, yDir)
            val line = lineX zip lineY
            println(line.size)
            addLine(acc, line)
          }
      }
  }

  println(allPoints.count(_._2 > 1))

}

/*


201,901 -> 170,932

31
-31

 */
