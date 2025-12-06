package y2025

import scala.io.Source

object Day02 extends App{
  val data = Source.fromResource("2025/2.data").getLines().toSeq.head.split(",")

  val part1 = data.map{x =>
    val bounds = x.split("-")
    val lower = bounds.head.toLong
    val upper = bounds.last.toLong
    sumMatches1(lower, upper, 0)
  }.sum

  println(part1)

  def sumMatches1(cur: Long, max:Long, acc: Long) : Long = {
    if(cur > max){
      acc
    }else{
      val s = cur.toString
      val str = s.splitAt(s.length / 2)
      if(str._1 == str._2){
        sumMatches1(cur + 1, max, acc + cur)
      }else{
        sumMatches1(cur + 1, max, acc)
      }
    }
  }

  val part2 = data.map { x =>
    val bounds = x.split("-")
    val lower = bounds.head.toLong
    val upper = bounds.last.toLong
    sumMatches2(lower, upper, 0)
  }.sum

  println(part2)

  def sumMatches2(cur: Long, max: Long, acc: Long): Long = {
    if (cur > max) {
      acc
    } else {
      val s = cur.toString
      val isInvalid = (1 to s.length / 2).exists{ size =>
        s.grouped(size).toSet.size == 1
      }
      if (isInvalid) {
        sumMatches2(cur + 1, max, acc + cur)
      } else {
        sumMatches2(cur + 1, max, acc)
      }
    }
  }


}
