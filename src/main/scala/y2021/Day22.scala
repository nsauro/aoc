package y2021

import scala.io.Source

object Day22 extends App {

  val data = Source.fromResource("2021/22.data").getLines()

  val actions = data.map {
    case Action.Off(xMin, xMax, yMin, yMax, zMin, zMax) =>
      Action.TurnOff(
        xMin.toInt,
        xMax.toInt,
        yMin.toInt,
        yMax.toInt,
        zMin.toInt,
        zMax.toInt
      )
    case Action.On(xMin, xMax, yMin, yMax, zMin, zMax) =>
      Action.TurnOn(
        xMin.toInt,
        xMax.toInt,
        yMin.toInt,
        yMax.toInt,
        zMin.toInt,
        zMax.toInt
      )
  }

  val turnedOn = actions.foldLeft(Set.empty[(Int, Int, Int)]) {
    case (acc, action) =>
      if (action.withinBounds(-50, 50)) {
        println(s"procesing $action")
        val coords: Seq[(Int, Int, Int)] = for {
          x <- action.xMin to action.xMax
          y <- action.yMin to action.yMax
          z <- action.zMin to action.zMax
        } yield {
          (x, y, z)
        }

        action match {
          case _: Action.TurnOn  => acc ++ coords
          case _: Action.TurnOff => acc.removedAll(coords)
        }
      } else {
        println(s"dropping action: $action")
        acc
      }
  }

  println(turnedOn.size)

  val (onActions, offActions) = actions.partition(_.isInstanceOf[Action.TurnOn])

  sealed trait Action {
    def xMin: Int

    def xMax: Int

    def yMin: Int

    def yMax: Int

    def zMin: Int

    def zMax: Int

    def withinBounds(min: Int, max: Int): Boolean = {
      withinBounds(xMin, xMax, min, max) &&
      withinBounds(yMin, yMax, min, max) &&
      withinBounds(zMin, zMax, min, max)
    }

    private def withinBounds(
        axisMin: Int,
        axisMax: Int,
        min: Int,
        max: Int
    ): Boolean = {
      axisMin >= min && axisMax <= max
    }
  }

  object Action {

    val Off =
      raw"""off x=([\-0-9]+)\.\.([\-0-9]+),y=([\-0-9]+)\.\.([\-0-9]+),z=([\-0-9]+)\.\.([\-0-9]+)""".r
    val On =
      raw"""on x=([\-0-9]+)\.\.([\-0-9]+),y=([\-0-9]+)\.\.([\-0-9]+),z=([\-0-9]+)\.\.([\-0-9]+)""".r

    case class TurnOn(
        xMin: Int,
        xMax: Int,
        yMin: Int,
        yMax: Int,
        zMin: Int,
        zMax: Int
    ) extends Action

    case class TurnOff(
        xMin: Int,
        xMax: Int,
        yMin: Int,
        yMax: Int,
        zMin: Int,
        zMax: Int
    ) extends Action

  }
  /*
  object Day22Part2 extends App {




    case class Cube(xMin : Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax : Int){


      def intersection(other : Cube) : Option[Cube] = {

        /*
        each axis has 4 options: equal, contained within, envelopes, overlaps
   */

        //x 1 10   -3 7             1,7
        // y 2 8    -4 6            2,6
        // z

      }



    }








  }*/

}
