package y2022

import scala.io.Source

object Day03 extends App{


  val data = Source.fromResource("2022/Day03").getLines()

  val lowerAsciiOffset = 'a'.toInt - 1
  val upperAsciiOffset = 'A'.toInt - 27

  println('a'.toInt - lowerAsciiOffset)
  println('A'.toInt - upperAsciiOffset)

  /*val sum = data.map{x =>
    val split = x.splitAt(x.length / 2)
    compute(split._1, split._2)
  }.sum

  println(sum)*/


  val sum2 = data.iterator.grouped(3).toSeq.map{ x =>
    println(x)
    compute2(x.map(_.toSet))
  }.sum

  println(sum2)


  val sum = data.map { x =>
    val split = x.splitAt(x.length / 2)
    compute(split._1, split._2)
  }.sum

  println(sum)

  def compute(first: String, second: String) : Int = {
    val common = first.toSet.intersect(second.toSet)
    common.map{ x =>
      val offset = if(x.isLower)lowerAsciiOffset else upperAsciiOffset
      x.toInt - offset
    }.sum
  }

  def compute2(elves : Seq[Set[Char]]) : Int = {
    elves.reduceLeft(_ intersect _).map{ x =>
      val offset = if (x.isLower) lowerAsciiOffset else upperAsciiOffset
      x.toInt - offset
    }.sum
  }

}
