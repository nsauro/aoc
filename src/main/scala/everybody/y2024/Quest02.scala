package everybody.y2024

import scala.io.Source

object Quest02 extends App:

  val data = Source.fromResource("everybody/2024/2-1.data").getLines().toSeq
  val data2 = Source.fromResource("everybody/2024/2-2.data").getLines().toSeq

 // part1(data)
  part2(data2)

  def part1(data : Seq[String]) : Unit = {
    val words = data.head.substring(6).split(",").map(_.r)
    val inscription = data.last
    val res = words.map(_.findAllMatchIn(inscription).toSeq.size).sum
    println(res)
  }

  def part2(data: Seq[String]): Unit = {



    val words = data.head.substring(6).split(",").map( x =>  s"$x|${x.reverse}".r)
    val inscriptions = data.tail
    val res = inscriptions.map{ inscription =>
      println(s"$inscription")
      val r = words.map(_.findAllMatchIn(inscription).map { x =>

        val r = x.end - x.start
        println(s"${x.matched} -- ${x.start} -- ${x.end}")
        r
      }.sum).sum
      println(r)
      r
    }.sum


  }

