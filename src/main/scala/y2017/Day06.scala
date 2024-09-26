package y2017

import scala.annotation.tailrec

object Day06 extends App:

  val buckets = "0\t5\t10\t0\t11\t14\t13\t4\t11\t8\t8\t7\t1\t4\t12\t11"
    .split("\\s")
    .filterNot(_ == "")
    .map(_.toInt)
  println(findLoop(0, Map(buckets.mkString("") -> 0)))

  @tailrec
  def findLoop(cycles: Int, seen: Map[String, Int]): Int = {
    val (value, i) = buckets.zipWithIndex.maxBy(_._1)
    buckets(i) = 0
    val allIncr = value / buckets.length
    val remainder = value % buckets.length
    buckets.mapInPlace(_ + allIncr)

    (1 to remainder).foreach { ri =>
      val newI =
        if i + ri < buckets.length then i + ri
        else (i + ri) % buckets.length
      buckets(newI) = buckets(newI) + 1
    }

    val newCycles = cycles + 1
    val str = buckets.mkString("")
    seen.get(str) match {
      case Some(c) => newCycles - c
      case None    => findLoop(newCycles, seen + (str -> newCycles))
    }
  }
