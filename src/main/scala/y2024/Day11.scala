package y2024

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Day11 extends App:

  val times = 75
  val input = ""
  val data = input.split("\\s+").map(x => (x.toLong, 1L)).toMap

  val t0 = System.currentTimeMillis()
  val res = doIt(data, times)
  println(s"ran in: ${System.currentTimeMillis() - t0}")
  println(res)

  
  def split(n : Long) : (Long, Long) = {
    val s = n.toString
    val parts = s.splitAt(s.length / 2)
    (parts._1.toLong, parts._2.toLong)
  }

  @tailrec
  def doIt(stones: Map[Long, Long], timesRemaining: Int) : Long = {
    if(timesRemaining == 0) then
      stones.values.sum
    else
      val newMap = stones.foldLeft(Map.empty[Long, Long]){ case(acc, (k,v)) =>
        if(k == 0) then
          acc + (1L -> (v + acc.getOrElse(1L, 0L)))
        else if ((math.log10(k).toInt + 1) % 2 == 0) then
          val splitted = split(k)
          val u1 = acc + (splitted._1 -> (v + acc.getOrElse(splitted._1, 0L)))
          u1 + (splitted._2 -> (v + u1.getOrElse(splitted._2, 0L)))
        else
          val newV = k * 2024L
          acc + (newV ->(v + acc.getOrElse(newV, 0L)))
      }
      doIt(newMap, timesRemaining - 1)
  }


