package y2025

import scala.io.Source

object Day05 extends App {
  val data = Source.fromResource("2025/5.data").getLines().toSeq
  val (rangesRaw, ingredientsRaw) = data.splitAt(data.indexOf(""))


  val ranges = rangesRaw.map{ x =>
    val (first, last) = x.splitAt(x.indexOf("-"))
    (first.toLong, last.tail.toLong)
  }

  val part1 = ingredientsRaw.tail.count{ r =>
    val l = r.toLong
    ranges.exists{ case (first, last) => l >= first && l <= last}
  }

  println(part1)

  val merged = mergeRanges(ranges, Seq.empty).map(x => x._2 - x._1 + 1).sum
  println(merged)

  def mergeRanges(ranges: Seq[(Long, Long)], acc: Seq[(Long, Long)]): Seq[(Long, Long)] = {
    if(ranges.isEmpty){
      acc
    }else{
      val (newRange, remaining) = mergeRanges(ranges.head, ranges.tail)
      mergeRanges(remaining, acc :+ newRange)
    }
  }

  def mergeRanges(range: (Long, Long), candidates:Seq[(Long, Long)]) : ((Long, Long), Seq[(Long, Long)]) = {

    val (mergeable, notMergeable) = candidates.partition{ x =>
      areMergeable(range, x)
    }
    if(mergeable.isEmpty){
      (range, candidates)
    }else{
      val mush = mergeable :+ range
      val newLower = mush.minBy(_._1)._1
      val newUpper = mush.maxBy(_._2)._2
      mergeRanges((newLower, newUpper), notMergeable)
    }

  }

  def areMergeable(source: (Long, Long), check: (Long, Long)): Boolean = {
    //check overlaps sources lower end
    check._1 <= source._1 && check._2 >= source._1  ||
      check._1 <= source._1 && check._2 >= source._2 ||  //check envelopes source
      check._1 >= source._1 && check._2 <= source._2  || //source evenlopes check
      check._1 <= source._2 && check._2 >= source._2
  }
}
