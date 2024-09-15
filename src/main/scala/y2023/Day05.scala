package y2023

import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
object Day05 extends App {

  val RangeHeader = raw"""(.+)-to-(.+) map:""".r
  val RangeData = raw"""(\d+) (\d+) (\d+)""".r

  val data = Source.fromResource("2023/5.data").getLines().toSeq

  val seeds = data.head.split(":").last.trim.split(" ").map(_.trim.toLong)

  val ranges = parseInput(data.tail, Seq.empty, None)

  ranges.foreach(println)

  val res = seeds.map { s =>
    ranges.foldLeft(Seq(s)) { case (acc, rm) => acc.flatMap(rm.mapValue) }.min
  }.min

  println(res)

  // part 2

  val res2F = Future.traverse(seeds.grouped(2)) { case x =>
    Future {
      println(s"processing: ${x.toSeq}")
      val res = findMin(x.head, x.last, 0, Long.MaxValue)
      println(s"finished ${x.toSeq}")
      res
    }
  }

  val part2Min = Await.result(res2F, 5.hours).toSeq
  println(part2Min.min)

  def findMin(start: Long, range: Long, cur: Long, curMin: Long): Long = {

    if (cur == range) {
      curMin
    } else {
      val loc = ranges
        .foldLeft(Seq(start + cur)) { case (acc, rm) =>
          acc.flatMap(rm.mapValue)
        }
        .min
      findMin(start, range, cur + 1, Math.min(curMin, loc))
    }
  }

  // stuff

  case class RangeMap(source: String, dest: String, ranges: Seq[Range]) {
    def mapValue(x: Long): Seq[Long] = {
      val ans = ranges.flatMap(_.mapValue(x))
      if (ans.isEmpty) {
        Seq(x)
      } else {
        ans
      }
    }

    def addRange(r: Range) = this.copy(ranges = ranges :+ r)
  }

  case class Range(destination: Long, source: Long, range: Long) {
    def mapValue(x: Long) = {
      val d = x - source
      Option.when(d <= range && d >= 0) {
        destination + d
      }
    }
  }

  def parseInput(
      i: Seq[String],
      acc: Seq[RangeMap],
      curr: Option[RangeMap]
  ): Seq[RangeMap] = {

    if (i.isEmpty) {
      acc ++ curr
    } else {
      i.head match {
        case RangeHeader(s, d) =>
          parseInput(i.tail, acc, Some(RangeMap(s, d, Seq.empty)))
        case RangeData(d, s, r) =>
          parseInput(
            i.tail,
            acc,
            curr.map(_.addRange(Range(d.toLong, s.toLong, r.toLong)))
          )
        case _ => parseInput(i.tail, acc ++ curr, None)
      }
    }
  }
}
