package everybody.y2024

import scala.io.Source

object Quest01 extends App:

  val values = Map(
    'A' -> 0,
    'B' -> 1,
    'C' -> 3,
    'D' -> 5,
    'x' -> 0
  )

  val data1 = Source.fromResource("everybody/2024/1-1.data").getLines().mkString("")
  val data2 = Source.fromResource("everybody/2024/1-2.data").getLines().mkString("")
  val data3 = Source.fromResource("everybody/2024/1-3.data").getLines().mkString("")

  println(getPotions(data1, 1))
  println(getPotions(data2, 2))
  println(getPotions(data3, 3))

  def getPotions(d: String, group: Int) : Int = {
    d.grouped(group).map { x =>
      val (xs, rest)  = x.partition(_ == 'x')
      val additional = Math.max(rest.length - 1, 0)
      if rest.isEmpty then
        0
      else
        rest.map(y => values(y) + additional).sum
    }.sum
  }



