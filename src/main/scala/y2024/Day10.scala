package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App:
  val data = Source.fromResource("2024/10.data").getLines().toArray.map(_.toCharArray)

  val part1Res = part1(0, 0, 0)
  println(part1Res)

  val part2Res = part2(0, 0, 0)
  println(part2Res)

  @tailrec
  def part1(x: Int, y: Int, acc: Long) : Long = {
    if !isValid(x, y) then
      acc
    else if data(y)(x) == '0' then
      val newAcc  = acc + getScore(Set((x,y)), Set.empty)
      val n = nextCoord(x, y)
      part1(n._1, n._2, newAcc)
    else
      val n = nextCoord(x, y)
      part1(n._1, n._2, acc)
  }

  @tailrec
  def part2(x: Int, y: Int, acc: Long): Long = {
    if !isValid(x, y) then
      acc
    else if data(y)(x) == '0' then
      val newAcc = acc + getScore2(Seq((x, y)), 0)
      val n = nextCoord(x, y)
      part2(n._1, n._2, newAcc)
    else
      val n = nextCoord(x, y)
      part2(n._1, n._2, acc)
  }

  @tailrec
  def getScore(queue: Set[(Int, Int)], peaksSeen: Set[(Int, Int)]) : Int = {
    if queue.isEmpty then
      peaksSeen.size
    else
      val next = queue.head
      val allNeighbors = neighbors(next._1, next._2)
      val (peaks, nonPeaks) = allNeighbors.partition(q => data(q._2)(q._1) == '9')
      getScore(queue.tail ++ nonPeaks, peaksSeen ++ peaks)
  }

  @tailrec
  def getScore2(queue: Seq[(Int, Int)], acc : Int): Int = {
    if queue.isEmpty then
      acc
    else
      val next = queue.head
      val allNeighbors = neighbors(next._1, next._2)
      val (peaks, nonPeaks) = allNeighbors.partition(q => data(q._2)(q._1) == '9')
      getScore2(queue.tail ++ nonPeaks, acc + peaks.size)
  }


  def nextCoord(x: Int, y: Int) = {
    if x == data.length - 1 then
      (0, y + 1)
    else
      (x + 1, y)
  }

  def neighbors(x: Int, y: Int ) = {
    Seq(
      (x + 1, y),
      (x -1 ,y),
      (x, y + 1),
      (x, y - 1)
    ).filter(p => isValid(p._1, p._2) && data(p._2)(p._1).isDigit && data(p._2)(p._1).asDigit - data(y)(x).asDigit == 1)
  }

  def isValid(x: Int, y: Int) = y < data.length && y >= 0 && x < data.head.length && x >= 0
