package y2022

import scala.io.Source

object Day18 extends App {

  val data: Set[(Int, Int, Int)] = Source
    .fromResource("2022/Day18")
    .getLines()
    .toSeq
    .map(_.split(","))
    .map(x => (x.head.toInt, x(1).toInt, x.last.toInt))
    .toSet

  val res = data.toSeq.map { p =>
    val adjacents = Seq(
      p.copy(_1 = p._1 + 1),
      p.copy(_1 = p._1 - 1),
      p.copy(_2 = p._2 + 1),
      p.copy(_2 = p._2 - 1),
      p.copy(_3 = p._3 + 1),
      p.copy(_3 = p._3 - 1)
    )
    val cnts = data.count(adjacents.contains)
    6 - cnts

  }.sum

  println(res)

  val exposedSides = data.toSeq.map { p =>
    val adjacents = Seq(
      p.copy(_1 = p._1 + 1),
      p.copy(_1 = p._1 - 1),
      p.copy(_2 = p._2 + 1),
      p.copy(_2 = p._2 - 1),
      p.copy(_3 = p._3 + 1),
      p.copy(_3 = p._3 - 1)
    )
    adjacents.forall(data.contains)
    val cnts = data.count(adjacents.contains)
    6 - cnts

  }

  val minX = data.minBy(_._1)

//  def

}
