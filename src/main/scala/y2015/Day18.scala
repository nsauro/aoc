package y2015

import scala.io.Source

object Day18 extends App {

  val data = Source.fromResource("2015/18.data").getLines().toSeq

  val mappedData = data.zipWithIndex.foldLeft(Map.empty[(Int, Int), Boolean]) {
    case (acc, (row, rIndex)) =>
      row.zipWithIndex.foldLeft(acc) { case (acc2, (char, cIndex)) =>
        acc2 + ((rIndex -> cIndex) -> (char == '#'))
      }
  }

  val maxRows = data.size
  val maxColumns = data.head.length
  val alwaysOn = Set(
    (0, 0),
    (0, maxColumns - 1),
    (maxRows - 1, 0),
    (maxRows - 1, maxColumns - 1)
  )

  // printGrid(toggleLights(mappedData, 4))

  val res = toggleLights(mappedData, 100)
  printGrid(res)
  println(res.count(_._2))

  def toggleLights(
      lights: Map[(Int, Int), Boolean],
      timesRemaining: Int
  ): Map[(Int, Int), Boolean] = {

    if (timesRemaining == 0) {
      lights
    } else {

      toggleLights(
        lights.foldLeft(Map.empty[(Int, Int), Boolean]) {
          case (acc, ((r, c), isOn)) =>
            val neighbors = Seq(
              (r - 1, c - 1),
              (r - 1, c),
              (r - 1, c + 1),
              (r, c - 1),
              (r, c + 1),
              (r + 1, c - 1),
              (r + 1, c),
              (r + 1, c + 1)
            )
            val onNeighbors = neighbors.count(x =>
              lights.contains(x) && (alwaysOn
                .contains(x) || lights.getOrElse(x, false))
            )

            if (alwaysOn.contains((r, c))) {
              acc + ((r -> c) -> true)
            } else if (isOn) {
              acc + ((r -> c) -> (onNeighbors == 2 || onNeighbors == 3))
            } else {
              acc + ((r -> c) -> (onNeighbors == 3))
            }
        },
        timesRemaining - 1
      )
    }

  }

  def printGrid(lights: Map[(Int, Int), Boolean]): Unit = {
    for {
      i <- 0 to maxRows
    } {
      val rowContents: Map[Int, Boolean] = lights.collect {
        case ((r, c), isOn) if r == i => (c, isOn)
      }
      println(
        rowContents.toSeq
          .sortBy(_._1)
          .map(x => if (x._2) '#' else '.')
          .mkString("")
      )
    }
  }

}
