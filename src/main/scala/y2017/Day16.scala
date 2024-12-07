package y2017

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day16 extends App:

  val data = Source.fromResource("2017/16.data").getLines().mkString("").split(",")

  val Spin = """s(\d+)""".r
  val Exchange = """x(\d+)/(\d+)""".r
  val Partner = """p([a-z])/([a-z])""".r

  val initial = Array('a', 'b', 'c','d','e','f','g','h','i','j','k','l','m','n','o','p')
  def initialMap() = scala.collection.mutable.LinkedHashMap(
    initial.zipWithIndex.toMap.toSeq*
  )
  val res = part1(data, initial)
  println(res.mkString(""))
  println(part2(0, data, initial, mutable.LinkedHashMap.empty).mkString(""))

  @tailrec
  def part1(ins: Seq[String], values : Array[Char]) : Array[Char] =
    if ins.isEmpty then
      values
    else
      ins.head match
        case Spin(amount) => 
          val parts = values.splitAt(values.length - amount.toInt)
          val updatedValues = Array.concat(parts._2, parts._1)
          part1(ins.tail, updatedValues)
        case Exchange(i1, i2) =>
          val temp = values(i1.toInt)
          values(i1.toInt) = values(i2.toInt)
          values(i2.toInt) = temp
          part1(ins.tail, values)
        case Partner(c1, c2) =>
          val i1 = values.indexOf(c1.head)
          val i2 = values.indexOf(c2.head)
          values(i2) = c1.head
          values(i1) = c2.head
          part1(ins.tail, values)


  @tailrec
  def part2(times: Int, ins: Seq[String], values : Array[Char], cache: mutable.LinkedHashMap[String, String]) : Array[Char] = {
    if(times == 1000000000) then
      values
    else
      val key = values.mkString("")
      println(key)
      cache.get(key) match{
        case Some(x) => part2(times + 1, ins, x.toArray, cache)
        case None =>
          val updated = part1(ins, values)
          cache.put(key, updated.mkString(""))
          part2(times + 1, ins, updated, cache)
      }
  }



