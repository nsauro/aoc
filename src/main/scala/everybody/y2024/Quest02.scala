/*package everybody.y2024

import scala.io.Source*/

/*object Quest02 extends App:

  val data = Source.fromResource("everybody/2024/2-1.data").getLines().toSeq
  val data2 = Source.fromResource("everybody/2024/2-2.data").getLines().toSeq

  println(part1(data))
  println(part2(data2))

  def part1(data : Seq[String]) = {
    val words = data.head.substring(6).split(",").map(_.r)
    val inscription = data.last
    words.map(_.findAllMatchIn(inscription).toSeq.size).sum
  }

  def part2(data: Seq[String]) = {

    val words = data.head.substring(6).split(",").map( x =>  s"$x|${x.reverse}".r)
    val inscriptions = data.tail
    inscriptions.map{ i =>
      i.find
      println(i)
      for {
        matches <- words.map(_.findAllMatchIn(i))
        mtch      <- matches
        m         <- mtch.
      }
      val r = words.flatMap(_.findAllMatchIn(i).map(m => (m.start to m.end).toSet)).length
      println(r)
      r
    }.sum


  }*/

