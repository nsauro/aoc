package y2017

import scala.io.Source

object Day11 extends App:
  val data = Source.fromResource("2017/11.data").mkString("").split(",")

  def ne(o : (Int, Int, Int)) = {
    o.copy(_2 = o._2 - 1, _3 = o._3 + 1)
  }

  def sw(o: (Int, Int, Int)) = {
    o.copy(_2 = o._2 + 1, _3 = o._3 - 1)
  }

  def nw(o: (Int, Int, Int)) = {
    o.copy(_1 = o._1 + 1, _3 = o._3 - 1)
  }

  def se(o: (Int, Int, Int)) = {
    o.copy(_1 = o._1 - 1, _3 = o._3 + 1)
  }

  def n(o: (Int, Int, Int)) = {
    o.copy(_1 = o._1 + 1, _2 = o._2 - 1)
  }

  def s(o: (Int, Int, Int)) = {
    o.copy(_1 = o._1 - 1, _2 = o._2 + 1)
  }

  val original = (0,0,0)
  val part1 = data.foldLeft(original){ case(acc, dir) =>
    dir match{
      case "ne" => ne(acc)
      case "sw" => sw(acc)
      case "nw" => nw(acc)
      case "se" => se(acc)
      case "n" => n(acc)
      case "s" => s(acc)
    }
  }

  val ans = Math.max(Math.max(Math.abs(part1._1), Math.abs(part1._2)),Math.abs(part1._3))
  println(ans)


  val part2 = data.foldLeft((original, 0)){ case((acc, max), dir) =>
    val newLoc = dir match {
      case "ne" => ne(acc)
      case "sw" => sw(acc)
      case "nw" => nw(acc)
      case "se" => se(acc)
      case "n" => n(acc)
      case "s" => s(acc)
    }
    val dis = Math.max(Math.max(Math.abs(newLoc._1), Math.abs(newLoc._2)),Math.abs(newLoc._3))
    (newLoc, Math.max(max, dis))
  }
  println(part2._2)

