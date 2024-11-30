package y2017

import scala.collection.mutable.ListBuffer

object Day15 extends App:

  val genAFactor = 16807
  val genBFactor = 48271
  val maxTimes = 5_000_000
  val divisor = 2147483647

  def part1(count : Int, a : Long, b: Long, matches : Int) : Int =

    if(count == maxTimes)
      matches
    else
      val nextA = (a * genAFactor) % divisor
      val nextB = (b * genBFactor) % divisor

      val  nextALower16 = nextA & ((1 << 16) - 1)
      val  nextBLower16 = nextB & ((1 << 16) - 1)

      val updated = if nextALower16 == nextBLower16 then matches + 1 else matches
      part1(count + 1, nextA, nextB, updated)


  println(part1(0, 618, 814, 0))


  def computeList(count : Int, current : Long, multiplier: Int, divisibleBy : Int, matches : ListBuffer[Long]) : ListBuffer[Long] = {
    if (matches.size == maxTimes)
      matches
    else
      val next = (current * multiplier) % divisor
      if (next % divisibleBy == 0)
        computeList(count + 1, next, multiplier, divisibleBy, matches.addOne(next))
      else
        computeList(count + 1, next, multiplier, divisibleBy, matches)

  }


  val aList = computeList(0, 618, genAFactor, 4, ListBuffer.empty)
  val bList = computeList(0, 814, genBFactor, 8, ListBuffer.empty)
  println(aList.size)
  println(bList.size)
  val part2  = aList.zip(bList).zipWithIndex.count{ case((a,b), i) =>
    val nextALower16 = a & ((1 << 16) - 1)
    val nextBLower16 = b & ((1 << 16) - 1)
    if nextBLower16 == nextALower16 then
      println(i)
    nextBLower16 == nextALower16
  }
  println(s"part 2: $part2")



