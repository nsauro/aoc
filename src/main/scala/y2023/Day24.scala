package y2023

import scala.io.Source

object Day24 extends App{

  val min = 200000000000000L
  val max = 400000000000000L

  val HailStoneParams = raw"""(-?\d+),\s*(-?\d+),\s*(-?\d+)\s*@\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)""".r

  val data = Source.fromResource("2023/24.data").getLines().toSeq.map{
    case HailStoneParams(x,y,z,xd,yd,zd) => HailStone(x.toDouble, y.toDouble, z.toDouble, xd.toDouble, yd.toDouble, zd.toDouble)
  }

  println(data.combinations(2).size)
  val res = data.combinations(2).toSeq.count { ps =>
    val intersection = ps.head.intersection(ps.last)
    intersection.exists{ case (x, y) =>
      x >= min && y >= min && x <= max && y<= max
    }
  }

  println(res)

  case class HailStone(x : Double, y : Double, z: Double, deltaX : Double, deltaY: Double, deltaZ: Double) {

    val slope = deltaY / deltaX

    def intersection(h: HailStone) : Option[(Double, Double)] = {
      val det = (-deltaX * h.deltaY) + (h.deltaX * deltaY)
      Option.when(det.abs > 1.0e-6){
        val detAt = (x * h.deltaY) + (-h.x * h.deltaY) + (-y * h.deltaX) + (h.y * h.deltaX)
        val detAs = (-y * deltaX) + (h.y * deltaX) + (x * deltaY) + (-h.x * deltaY)
        val t = detAt / det
        val s = detAs / det
        Option.when(t > 0 && s > 0) {
          (x + deltaX * t, y + deltaY * t)
        }
      }
    }.flatten

  }

  //part 2: just usd someone's sympy solution...i really don't care to solve this super difficult question
}
