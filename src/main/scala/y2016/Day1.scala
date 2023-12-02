package y2016

import scala.io.Source

object Day1 extends App {

  val data = Source.fromResource("2016/1.data").getLines().toSeq.mkString.split(", ")

  val Step = raw"""([RL])(\d+)""".r


  val finalDest = move((0,0), 0, data)

  println(finalDest)
  println((Math.abs(finalDest._1) + Math.abs(finalDest._2)))

  val finalDest2 = move2((0, 0), 0, data, Set.empty)

  println(finalDest2)
  println((Math.abs(finalDest2._1) + Math.abs(finalDest2._2)))


  def move(cur: (Int, Int), heading: Int, steps: Seq[(String)]) : (Int, Int) = {

    steps.headOption match {
      case Some(Step(dir,amt)) => {
        val tempHeading = if(dir == "R") heading + 90 else heading - 90
        val newHeading = if(tempHeading > 270) 0 else if (tempHeading < 0) 270 else tempHeading

        val iAmt = amt.toInt
        newHeading match {
          case 0 => move(cur.copy(_2 = cur._2 + iAmt), newHeading, steps.tail)
          case 90 =>move(cur.copy(_1 = cur._1 + iAmt), newHeading, steps.tail)
          case 180 =>move(cur.copy(_2 = cur._2 - iAmt), newHeading, steps.tail)
          case 270 => move(cur.copy(_1 = cur._1 - iAmt), newHeading, steps.tail)
        }
      }
      case None => cur
    }

  }

  def move2(cur: (Int, Int), heading: Int, steps: Seq[(String)], visited: Set[(Int, Int)]): (Int, Int) = {

    steps.headOption match {
      case Some(Step(dir, amt)) => {
        val tempHeading = if (dir == "R") heading + 90 else heading - 90
        val newHeading = if (tempHeading > 270) 0 else if (tempHeading < 0) 270 else tempHeading

        val iAmt = amt.toInt
        val newLocations = newHeading match {
          case 0 =>  (1 to iAmt).map(x => cur.copy(_2 = cur._2 + x))
          case 90 => (1 to iAmt).map(x => cur.copy(_1 = cur._1 + x))
          case 180 => (1 to iAmt).map(x => cur.copy(_2 = cur._2 - x))
          case 270 => (1 to iAmt).map(x => cur.copy(_1 = cur._1 - x))
        }
        val alreadyVisited = newLocations.find(visited.contains)
        alreadyVisited match {
          case Some(x) => x
          case None => move2(newLocations.last, newHeading, steps.tail, visited ++ newLocations)
        }
      }
      case None => cur
    }

  }

}
