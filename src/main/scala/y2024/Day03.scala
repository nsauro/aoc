package y2024

import scala.io.Source

object Day03 extends App:
  val data = Source.fromResource("2024/3.data").getLines().mkString("")

  val Mul = """mul\((\d+),(\d+)\)""".r
  val MulPart2 = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r


  val part1 = Mul.findAllMatchIn(data).map{x =>
    x.group(1).toInt * x.group(2).toInt
  }.sum

  println(part1)

  val part2 = MulPart2.findAllMatchIn(data).foldLeft((true, 0)){
    case((enabled, m), mtch) =>
      if mtch.group(0) == "do()" then
        (true,m)
      else if mtch.group(0) == "don't()" then
        (false,m)
      else if !enabled then
        (enabled, m)
      else
        val mult = mtch.group(1).toInt * mtch.group(2).toInt
        (enabled, m + mult)
  }
  println(part2._2)
