package y2024

import scala.io.Source

object Day05 extends App:

  val data = Source.fromResource("2024/5.data").getLines().toSet
  val updates = Source.fromResource("2024/5.updates.data").getLines().toSeq.map(_.split(",").map(_.toInt).toSeq)

  val part1 = updates.map(isValid).sum
  println(part1)

  val part2 = updates.filter(x => isValid(x) == 0).map(y => putInOrder(y.toSet, Seq.empty)).sum
  println(part2)

  def isValid(s : Seq[Int]) : Int = {
    val res = s.sliding(2).forall{ x =>
      data.contains(x.mkString("|"))
    }
    if(res) then
      s((s.length -1) / 2)
    else
      0
  }

  def putInOrder(unordered : Set[Int], ordered : Seq[Int]) : Int = {
    if(unordered.isEmpty) then
      ordered((ordered.length -1) / 2)
    else
      val next = unordered.find { x =>
        val wo = unordered.filter(_ != x)
        val isAfterOrdered = ordered.forall(o => data.contains(s"$o|$x"))
        val isBeforeUnordered = wo.forall(o => data.contains(s"$x|$o"))
        isAfterOrdered && isBeforeUnordered
      }.get
      putInOrder(unordered - next, ordered :+ next)

  }

