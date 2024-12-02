package y2024

import scala.io.Source

object Day01 extends App:

  val data = Source.fromResource("2024/1.data").getLines().toArray
  val arrays = data.map(_.split(" ").filter(_ != "").map(_.toInt))
  val lists = arrays.transpose
  val first = lists.head.sorted
  val last = lists.last.sorted

  val part1 = first.zip(last).map{case (a,b) => Math.abs(b - a)}.sum
  println(part1)


  val mappedSecond = last.groupMapReduce(identity)(_ => 1)(_ + _)
  val part2 = first.map{ x =>
    x * mappedSecond.getOrElse(x, 0)
  }.sum

  println(part2)

