package y2016

import scala.annotation.tailrec
import scala.io.Source

object Day20 extends App {

  val data = Source.fromResource("2016/20.data").getLines().toSeq
  val formatter = java.text.NumberFormat.getIntegerInstance

  val ranges = data.map { x =>
    val pieces = x.split("-")
    pieces.head.toLong -> pieces.last.toLong
  }.toMap

  // println(findSmallestGap(0, 4294967295L, ranges))
  println(findAllGaps(0, 4294967295L, ranges, 0))

  @tailrec
  def findSmallestGap(
      start: Long,
      finish: Long,
      remaining: Map[Long, Long]
  ): Long = {
    println(
      f"remaining ranges: ${remaining.size} : start: ${formatter.format(start)}"
    )
    if (start > finish || remaining.isEmpty) {
      start
    } else {
      remaining.get(start) match {
        case Some(end) => {
          println(
            f"found: ${formatter.format(start)} - ${formatter.format(end)}"
          )
          findSmallestGap(end + 1, finish, remaining.removed(start))
        }
        case None => {
          println(f"no hit found for ${formatter.format(start)}")
          val overlapKVs = remaining.filter(x => x._1 < start)
          val overlaps = overlapKVs.values
          if (overlaps.isEmpty) {
            start
          } else {
            println(
              f"found overlaps: $overlapKVs, using ${formatter.format(overlaps.max)}"
            )
            findSmallestGap(
              overlaps.max + 1,
              finish,
              remaining.removedAll(overlapKVs.keys)
            )
          }
        }
      }
    }
  }

  @tailrec
  def findAllGaps(
      start: Long,
      finish: Long,
      remaining: Map[Long, Long],
      count: Long
  ): Long = {
    println("--------------")
    println(
      f"remaining ranges: ${remaining.size} : start: ${formatter.format(start)}, count: $count"
    )
    if (start > finish) {
      count
    } else {
      remaining.get(start) match {
        case Some(end) => {
          println(
            f"found: ${formatter.format(start)} - ${formatter.format(end)}"
          )
          findAllGaps(end + 1, finish, remaining.removed(start), count)
        }
        case None => {
          // println(s"no hit found for $start")
          val nextUp = remaining.filter(x => x._2 >= start)
          val overlapKVs = nextUp.filter(x => x._1 < start)
          val overlaps = overlapKVs.values
          val str = overlapKVs.map { case (k, v) =>
            s"${formatter.format(k)} - ${formatter.format(v)}"
          }
          if (overlaps.isEmpty) {
            println(
              f"gap found: ${formatter.format(start)}, current gaps: $count"
            )

            val next = nextUp.minBy(_._1)
            println(f"jumping to ${next}")
            println(f"count + (next._1 - start = ${count + (next._1 - start)}")
            findAllGaps(next._1, finish, nextUp, count + (next._1 - start))
          } else {
            println(
              f"found overlaps: $str, using ${formatter.format(overlaps.max)}"
            )
            findAllGaps(
              overlaps.max + 1,
              finish,
              nextUp.removedAll(overlapKVs.keys),
              count
            )
          }
        }
      }
    }
  }
}
//10   20
