package y2016

import scala.io.Source

object Day3 extends App{

  val data = Source.fromResource("2016/3.data").getLines().toSeq.map(_.split("  ").collect{case s if s.trim != "" => s.trim.toInt}.toSeq)

  val x  = data.transpose

  println(x.map(_.grouped(3).count(isTriangle)).sum)
  println(data.count(isTriangle))

  def isTriangle(s : Seq[Int]) : Boolean = {
    s(0) + s(1)  > s(2) && s(0) + s(2) > s(1) && s(1) + s(2) > s(0)
  }

}
