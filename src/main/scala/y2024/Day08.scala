package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends App:

  val data = Source.fromResource("2024/8.data").getLines().toArray.map(_.toCharArray)

  val freqs = buildFrequencyMap(0,0, Map.empty)

  println(freqs)

  val part1 = freqs.values.flatMap{ ps =>
    ps.combinations(2).flatMap{ x =>
      val f = x.head
      val l = x.last
      val xd = f._1 - l._1
      val yd = f._2 - l._2
      val res = Seq((l._1 - xd, l._2 - yd), (f._1 + xd, f._2 +yd)).filter(c => isValid(c._1, c._2))
      res
    }.distinct
  }.toSet.size

  println(part1)


  val part2 = freqs.values.flatMap { ps =>
    ps.combinations(2).flatMap { x =>
      val f = x.head
      val l = x.last
      computeAntinodes(f._1, f._2, l._1 - f._1, l._2 - f._2, Seq.empty)
        ++ computeAntinodes(l._1, l._2, f._1 - l._1, f._2 - l._2, Seq.empty) ++ ps.toSet
    }.distinct
  }.toSet.size

  println(part2)

  @tailrec
  def computeAntinodes(x: Int, y: Int, xd: Int, yd: Int, acc : Seq[(Int, Int)]) : Seq[(Int, Int)] = {
    val nextX = x - xd
    val nextY = y - yd
    if !isValid(nextX, nextY) then
      acc
    else
      computeAntinodes(nextX, nextY, xd, yd, acc :+ (nextX, nextY))

  }

  @tailrec
  def buildFrequencyMap(x:Int, y:Int, acc: Map[Char, Seq[(Int, Int)]]): Map[Char, Seq[(Int, Int)]] = {
    if !isValid(x, y) then
      acc
    else if data(y)(x) != '.' then
      val c = data(y)(x)
      val s = acc.getOrElse(c, Seq.empty[(Int, Int)])
      val updated = acc + (c -> (s :+ ((x,y))))
      val n = nextCoord(x,y)
      buildFrequencyMap(n._1, n._2, updated)
    else
      val n = nextCoord(x, y)
      buildFrequencyMap(n._1, n._2, acc)
  }



  def nextCoord(x: Int, y: Int) = {
    if x == data.length - 1 then
      (0, y + 1)
    else
      (x + 1, y)
  }

  def isValid(x: Int, y: Int) = y < data.length && y >= 0 && x < data.head.length && x >= 0
