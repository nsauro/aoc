package y2022

import scala.io.Source

object Day09 extends App {
  val data = Source.fromResource("2022/Day09").getLines().toSeq

  val first = Movement(data.head)
  val visited = compute(
    data.tail,
    first,
    (10000, 10000),
    (10000, 10000),
    Set((10000, 10000))
  )
  println(visited.size)

  val largerVisited = compute2(
    data.tail,
    first,
    Seq.fill(10)((10000, 10000)),
    Set((10000, 10000))
  )
  println(largerVisited.size)

  case class Movement(dir: String, steps: Int)

  object Movement {
    def apply(str: String): Movement = {
      val split = str.split(" ")
      Movement(split.head, split.last.toInt)
    }
  }

  def compute(
      movements: Seq[String],
      curMove: Movement,
      h: (Int, Int),
      t: (Int, Int),
      tailVisited: Set[(Int, Int)]
  ): Set[(Int, Int)] = {

    if (movements.isEmpty && curMove.steps == 0) {
      tailVisited
    } else if (curMove.steps == 0) {
      val nextInstr = Movement(movements.head)
      compute(movements.tail, nextInstr, h, t, tailVisited)
    } else {

      val (newH, newT) = curMove.dir match {
        case "D" => {
          val h2 = (h._1, h._2 - 1) // -2
          val t2 = if (Math.abs(h2._2 - t._2) > 1) {
            (h2._1, h2._2 + 1)

          } else {
            t
          }
          (h2, t2)
        }
        case "U" => {
          val h2 = (h._1, h._2 + 1)
          val t2 = if (Math.abs(h2._2 - t._2) > 1) {
            (h2._1, h2._2 - 1)

          } else {
            t
          }
          (h2, t2)
        }
        case "L" => {
          val h2 = (h._1 - 1, h._2) // -2
          val t2 = if (Math.abs(h2._1 - t._1) > 1) {
            (h2._1 + 1, h2._2)

          } else {
            t
          }
          (h2, t2)
        }
        case "R" => {
          val h2 = (h._1 + 1, h._2) // -2
          val t2 = if (Math.abs(h2._1 - t._1) > 1) {
            (h2._1 - 1, h2._2)

          } else {
            t
          }
          (h2, t2)
        }
      }

      compute(
        movements,
        curMove.copy(steps = curMove.steps - 1),
        newH,
        newT,
        tailVisited + newT
      )
    }
  }

  def compute2(
      movements: Seq[String],
      curMove: Movement,
      knots: Seq[(Int, Int)],
      tailVisited: Set[(Int, Int)]
  ): Set[(Int, Int)] = {

    if (movements.isEmpty && curMove.steps == 0) {
      tailVisited
    } else if (curMove.steps == 0) {
      val nextInstr = Movement(movements.head)
      compute2(movements.tail, nextInstr, knots, tailVisited)
    } else {

      val newH = curMove.dir match {
        case "D" => {
          (knots.head._1, knots.head._2 - 1) // -2

        }
        case "U" => {
          (knots.head._1, knots.head._2 + 1) // -2

        }
        case "L" => {
          (knots.head._1 - 1, knots.head._2) // -2
        }
        case "R" => {
          (knots.head._1 + 1, knots.head._2) // -2
        }
      }
      val newKnots = knots.tail.foldLeft(Seq(newH)) { case (acc, t) =>
        acc :+ updatePoint(acc.last, t)

      }
      compute2(
        movements,
        curMove.copy(steps = curMove.steps - 1),
        newKnots,
        tailVisited + newKnots.last
      )
    }
  }

  def updatePoint(h: (Int, Int), t: (Int, Int)): (Int, Int) = {

    def getValue(i: Int): Int = {
      if (i == -2) {
        -1
      } else if (i == 2) {
        1
      } else {
        i
      }
    }

    // -2,-1,0,1,2
    val diffX = h._1 - t._1
    val diffY = h._2 - t._2

    if (Math.abs(diffX) <= 1 && Math.abs(diffY) <= 1) {
      t
    } else {
      (t._1 + getValue(diffX), t._2 + getValue(diffY))

    }

  }

}
