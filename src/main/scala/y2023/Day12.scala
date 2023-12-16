package y2023

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day12 extends App{

  val data = Source.fromResource("2023/12.data").getLines().toSeq

  /*val res = data.map{ x =>
    val s = x.split(" ")
    val springs = s.head.trim
    val counts = s.last.split(",").map(_.trim.toInt)
    val n = springs.count(_ == '?')
    val possibilities = Math.pow(2, n)
    val r =  0 until possibilities.toInt
    //println(s"parsing: ${s.head} -- ${counts.mkString(",")} -- ${n}")
    val bla = r.count{ p =>
      val s =  p.toBinaryString
      val padding = new String(Array.fill(n - s.length)('0'))
      val opt = merge(springs.toArray, s.prependedAll(padding))
    //  println(opt.mkString(""))
      val m = matches(opt, counts)
     // println(s"matches: $m")
      m
    }
    println(s"matches: $bla")
    bla
  }.sum


  println(res)*/

  /*val res2 = data.map{ x =>
    val s = x.split(" ")
    val springs = s.head.trim
    val expandedSprings = springs  ///Seq.fill(5)(springs).mkString("?")
    val expandedCounts = s.last.trim /// Seq.fill(5)(s.last.trim).mkString(",")
    println(expandedSprings)
    println(expandedCounts)
    val qLocs = expandedSprings.zipWithIndex.collect{
      case ('?', i) => i
    }
    val res = dfs(expandedSprings.toArray, qLocs, expandedCounts.split(",").map(_.trim.toInt), 0, 0)
    println(res)
    res
  }.sum
  println(res2)*/

  def matches(s : Array[Char], groups : Seq[Int]) : Boolean = {
    println(s"${s.mkString("")} -- ${groups.mkString(",")}" )
    val l = ListBuffer.empty[Int]
    for{
      i <- s.indices
    } {
      if(i == 0){
        if(s(i) == '#')  l.addOne(1)
      }else{
        val prev = s(i - 1)
        val cur = s(i)
        (prev,cur) match {
          case ('#', '#') => l(l.size - 1) = l.last + 1
          case ('.', '.') =>
          case ('.', '#') => l.addOne(1)
          case ('#', '.') =>
        }
      }
    }
    l == groups

  }

  def merge(s : Array[Char], subs : String) = {
    val q = mutable.Queue.from(subs)
    s.mapInPlace{
      case '?' => {
        val next = q.dequeue()
        next match{
          case '1' => '#'
          case _ => '.'
        }
      }
      case o => o
    }

  }

  /*def dfs(s: Array[Char], qLocs:Seq[Int], group: Array[Int], q: Int, acc: Int) : Int = {
    s(qLocs(q)) = '.'
    val qres = check2(s, group, 0,0) match{
      case Valid => 1
      case Invalid => 0
      case Inconclusive => dfs(s, qLocs, group, q + 1, acc)
    }
    s(qLocs(q)) = '#'
    val pres = check2(s, group, 0,0) match {
      case Valid => 1
      case Invalid => 0
      case Inconclusive => dfs(s, qLocs, group, q + 1, acc)
    }
    pres + acc + qres
  }*/



  /*def check2(s: Array[Char], groups: Array[Int], curG: Int, visitedPs: mutable.HashSet[Int]): Validity = {
    println(s"${s.mkString("")} -- ${groups.mkString(",")} -- $visitedPs -- $curG")

    if(!s.contains('?')){
      val r = matches(s, groups)
      if(r){
        println("valid")
        Valid
      }else{
        println("invalid")
        Invalid
      }
    }else{
     // s.
      //just need to determine invalid
      val firstQ = s.indexOf('?')
      val firstP = s.indexWhere( x => x == '#')
      if(firstQ < firstP){
        Inconclusive
      }else{

      }


      }
    }
  }*/
}


sealed trait Validity

case object Valid extends Validity
case object Invalid extends Validity
case object Inconclusive extends Validity