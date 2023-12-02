package y2016

import scala.io.Source


object Day6 extends App{

  val data = Source.fromResource("2016/6.data").getLines().toSeq.map(_.toCharArray)

  val ans = data.transpose.map { x =>
    x.groupBy(identity).minBy(_._2.size)._1
  }.mkString("")
  println(ans)


}
