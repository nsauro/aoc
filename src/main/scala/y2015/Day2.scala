package y2015

import scala.io.Source

object Day2 extends App {

  val data = Source.fromResource("2015/2.data").getLines()

  val res = data.foldLeft((0L, 0L)) {
    case ((wp, rb), x) => {
      val parts = x.split("x")
      val l = parts(0).toInt
      val w = parts(1).toInt
      val h = parts(2).toInt
      val lw = l * w
      val wh = w * h
      val hl = h * l
      val area = (2 * lw) + (2 * wh) + (2 * hl)
      val smallest = lw.min(wh).min(hl)
      val newWp = wp + area + smallest

      val smallestSides = Seq(l, w, h).sorted.take(2).map(_ * 2).sum
      val bow = l * w * h
      (newWp, rb + smallestSides + bow)

    }
  }

  println(res)

}
