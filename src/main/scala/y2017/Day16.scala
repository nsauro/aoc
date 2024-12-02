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
  val res = part1(data, initial, initialMap())
  println(res.mkString(""))
  println(part2(0, data, initial, initialMap()).mkString(""))

  @tailrec
  def part1(ins: Seq[String], values : Array[Char], locations: mutable.LinkedHashMap[Char, Int]) : Array[Char] =
    if ins.isEmpty then
      values
    else
      ins.head match
        case Spin(amount) => 
          val parts = values.splitAt(values.length - amount.toInt)
          val updatedValues = Array.concat(parts._2, parts._1)
          val updatedLocations = updateMap(updatedValues, locations)
          part1(ins.tail, updatedValues, updatedLocations)
        case Exchange(i1, i2) =>
          val temp = values(i1.toInt)
          values(i1.toInt) = values(i2.toInt)
          values(i2.toInt) = temp
          val updatedLocations = updateMap(values, locations)
          part1(ins.tail, values, updatedLocations)
        case Partner(c1, c2) =>
          val i1 = locations(c1.head)
          val i2 = locations(c2.head)
          values(i2) = c1.head
          values(i1) = c2.head
          part1(ins.tail, values, updateMap(values, locations))


  @tailrec
  def part2(times: Int, ins: Seq[String], values : Array[Char], locations: mutable.LinkedHashMap[Char, Int]) : Array[Char] = {
    //#if times > 10000 && times % 10000 == 0 then
    println(times)

    if(times == 1_000_000_000) then
      values
    else
      val updated = part1(ins, values, locations)
      val newLocs = updateMap(updated, locations)
      part2(times + 1, ins, updated, newLocs)
  }

  def updateMap(values: Array[Char], locations: mutable.LinkedHashMap[Char, Int]) : mutable.LinkedHashMap[Char, Int] =
    values.zipWithIndex.foreach{ case(c, i) =>
      locations.put(c, i)
    }
    locations



