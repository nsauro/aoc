package y2024

import scala.collection.mutable
import scala.io.Source

object Day19 extends App:

  val patterns: mutable.LinkedHashSet[String] = mutable.LinkedHashSet.from(Source
    .fromResource("2024/19.patterns.data")
    .getLines()
    .mkString("")
    .split(", ")
    .toSet)

  val towels = Source.fromResource("2024/19.towels.data").getLines().toSeq
  val part1Res = towels.count { x =>
    canMakeDP(x)
  }
  println(part1Res)

  val part2Res = towels.map { x =>
    canMakeCountDP(x)
  }.sum
  println(part2Res)

  def canMakeDP(s: String) : Boolean = {
    val dpArr = Array.fill(s.length + 1)(false)
    dpArr(0) = true
    for {
      i <- 1 to s.length
      j <- 0 until i
    } {
      if !dpArr(i) then
        if dpArr(j) && patterns.contains(s.substring(j,i)) then
          dpArr(i) = true
    }
    dpArr(s.length)
  }

  def canMakeCountDP(s: String): Long = {
    val dpArr = Array.fill(s.length + 1)(0L)
    dpArr(0) = 1
    for {
      i <- 1 to s.length
      j <- 0 until i
    } {
      if patterns.contains(s.substring(j, i)) then
          dpArr(i) = dpArr(i) + dpArr(j)
    }
    dpArr(s.length)
  }

