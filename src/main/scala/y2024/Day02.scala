package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day02 extends App:
  val data = Source.fromResource("2024/2.data").getLines().toSeq


  val part1 = data.count{ x =>
    val ints = x.split(" ").map(_.toInt)
    isSafe(ints)
  }
  println(part1)


  val part2 = data.count { x =>
    val ints = x.split(" ").map(_.toInt)
    if isSafe(ints) then
      true
    else
       dampener(ints)

  }
  println(part2)


  def dampener(ints: Array[Int]) : Boolean = {

    @tailrec
    def tryWithout(i : Int) : Boolean = {
      if(i == ints.length) then
        false
      else
        val newArray = ints.zipWithIndex.collect{
          case (v, j) if j != i => v
        }
        val isNowSafe = isSafe(newArray)
        if isNowSafe then
          true
        else
          tryWithout(i + 1)
    }
    tryWithout(0)
  }

  def isSafe(ints : Array[Int]) : Boolean = {
    val inc = ints.sorted
    val dec = ints.sortBy(a => -a)
    ((ints.sameElements(inc)) || (ints.sameElements(dec))) && ints.sliding(2).forall { p =>
      val diff = Math.abs(p.head - p.last)
      diff >= 1 && diff <= 3
    }
  }
