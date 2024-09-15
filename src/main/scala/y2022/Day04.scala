package y2022

import scala.io.Source

object Day04 extends App {

  val data = Source.fromResource("2022/Day04").getLines().toSeq

  val Input = raw"""(\d+)-(\d+),(\d+)-(\d+)""".r
  println(Input.matches(data.head))

  val count = data.count {
    case Input(fmin, fmax, smin, smax) => {
      isContained2(fmin.toInt, fmax.toInt, smin.toInt, smax.toInt)
    }
  }

  println(count)

  def isContained(fmin: Int, fmax: Int, smin: Int, smax: Int): Boolean = {
    (fmin >= smin && fmax <= smax) || (smin >= fmin && smax <= fmax)

  }

  def isContained2(fmin: Int, fmax: Int, smin: Int, smax: Int) = {
    (fmin.to(fmax).toSet intersect (smin.to(smax)).toSet).nonEmpty
  }

}
