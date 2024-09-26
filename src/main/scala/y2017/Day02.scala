package y2017

import scala.io.Source

object Day02 extends App:

  val data = Source.fromResource("2017/2.data").getLines().toSeq

  val checksum = data.map { row =>
    val sorted =
      row.split("\\s+").map(_.trim).filter(_ != "").map(_.toInt).sorted
    sorted.last - sorted.head
  }.sum

  println(checksum)

  val checksum2 = data.map { row =>
    val all =
      row.split("\\s+").map(_.trim).filter(_ != "").map(_.toInt).combinations(2)
    all.collectFirst { case Divisible(v) => v }.getOrElse(0)
  }.sum

  println(checksum2)

  object Divisible:
    def unapply(a: Array[Int]): Option[Int] = {
      if (a.length == 2) then
        val f = Math.max(a.head, a.last)
        val l = Math.min(a.head, a.last)
        Option.when(f % l == 0) {
          f / l
        }
      else None
    }
