package y2023

import scala.io.Source

object Day08 extends App {

  val data = Source.fromResource("2023/8.data").getLines().toSeq

  val instructions = data.head

  val N = raw"""([A-Z0-9]{3}) = \(([A-Z0-9]{3}), ([A-Z0-9]{3})\)""".r

  val nodeMap = data.tail.foldLeft(Map.empty[String, (String, String)]) {
    case (acc, l) =>
      l match {
        case N(a, b, c) => acc + (a -> (b, c))
        case _          => acc
      }
  }

  println(doIt("AAA", "ZZZ", 0))

  val start = nodeMap.keySet.filter(_.endsWith("A")).toSeq

  val ranges = findCounts(start, 0, Seq.empty)

  val res2 = ranges.reduce(lcm)
  println(res2)

  def lcm(a: Long, b: Long): Long =
    (a * b) / gcd(Math.max(a, b), Math.min(a, b))

  // euclid
  def gcd(a: Long, b: Long): Long = {
    if (b == 0) {
      a
    } else {
      val r = a % b
      gcd(b, r)
    }
  }

  def doIt(cur: String, dest: String, steps: Long): Long = {
    if (cur == dest) {
      steps
    } else {
      val i = (steps % instructions.length).toInt
      val n = nodeMap(cur)
      val next = if (instructions(i) == 'L') n._1 else n._2
      doIt(next, dest, steps + 1)
    }
  }
  def findCounts(
      s: Iterable[String],
      steps: Long,
      acc: Seq[Long]
  ): Seq[Long] = {
    val (done, cont) = s.partition(_.endsWith("Z"))
    val updatedAcc = if (done.nonEmpty) acc :+ steps else acc
    if (cont.isEmpty) {
      updatedAcc
    } else {
      val i = steps % instructions.length
      val next = cont.map { x =>
        val n = nodeMap(x)
        if (instructions(i.toInt) == 'L') n._1 else n._2
      }
      findCounts(next, steps + 1, updatedAcc)
    }
  }

}
