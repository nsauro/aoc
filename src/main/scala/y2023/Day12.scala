package y2023

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day12 extends App {

  val data = Source.fromResource("2023/12.data").getLines().toSeq

  val res = data.map { x =>
    val s = x.split(" ")
    val springs = s.head.trim
    val counts = s.last.split(",").map(_.trim.toInt)
    val n = springs.count(_ == '?')
    val possibilities = Math.pow(2, n)
    val r = 0 until possibilities.toInt
    val res = r.count { p =>
      val s = p.toBinaryString
      val padding = new String(Array.fill(n - s.length)('0'))
      val opt = merge(springs.toArray, s.prependedAll(padding))
      matches(opt, counts)
    }
   // println(s"$springs -- $res")
    res
  }.sum


  println(res)
  val cache = mutable.HashMap.empty[(String, Seq[Int]), Long]

  val expanded = data.map{ x =>
    val s = x.split(" ")
    val springs = s.head.trim
    val counts = s.last.split(",").map(_.trim.toInt)
    val expandedSprings = Seq.fill(5)(springs).mkString("?")
    val expandedCounts = counts ++ counts ++ counts ++ counts ++ counts
    val res = part2(expandedSprings, expandedCounts)
    //println(s"$x - $res")
    //cache.toSeq.sortBy(_._1._1.length).foreach(println)
    res

  }.sum

  println(expanded)

  def matches(s: Array[Char], groups: Seq[Int]): Boolean = {
    val l = ListBuffer.empty[Int]
    for {
      i <- s.indices
    } {
      if (i == 0) {
        if (s(i) == '#') l.addOne(1)
      } else {
        val prev = s(i - 1)
        val cur = s(i)
        (prev, cur) match {
          case ('#', '#') => l(l.size - 1) = l.last + 1
          case ('.', '.') =>
          case ('.', '#') => l.addOne(1)
          case ('#', '.') =>
        }
      }
    }
    l == groups

  }

  def merge(s: Array[Char], subs: String) = {
    val q = mutable.Queue.from(subs)
    s.mapInPlace {
      case '?' => {
        val next = q.dequeue()
        next match {
          case '1' => '#'
          case _ => '.'
        }
      }
      case o => o
    }
  }



  def part2(s : String, groups: Seq[Int]) : Long = {
    val k = (s, groups)
    if(cache.contains(k)){
      cache(k)
    }else{
      val res =
        if(s.isEmpty){ //remaining groups..no chars
          if(groups.isEmpty) 1L else 0L
      }else if(groups.isEmpty){  ///fully exhausted
        if(!s.contains('#')) 1L else 0L
      }else{
        s.head match{
          case '#'  if s.length >= groups.head => {
            val (m, remaining) = s.splitAt(groups.head)
            if(m.exists(_ == '.') || (remaining.nonEmpty && remaining.head == '#')){
              0L
            }else{
              part2(remaining.tail, groups.tail)
            }
          }
          case '#' => 0
          case '?' => {
            part2("." + s.tail, groups) + part2("#" + s.tail, groups)
          }
          case '.' => part2(s.tail, groups)
        }
      }
      cache.addOne(k, res)
      res
    }

  }

}