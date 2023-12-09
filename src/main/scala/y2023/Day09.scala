package y2023

import scala.io.Source

object Day09 extends App{

  val data = Source.fromResource("2023/9.data").getLines().toSeq.map(_.split(" ").toSeq.map(_.trim.toInt))

  println(data.map(doIt(_, Seq.empty)).sum)
  println(data.map(doIt2(_, Seq.empty)).sum)


  def doIt(s : Seq[Int], acc : Seq[Int]) : Int = {
    val reduced = s.sliding(2).map(x => x.last - x.head).toSeq
    val updatedAcc = acc :+ s.last
    if(reduced.toSet == Set(0)){
      updatedAcc.sum
    }else{
      doIt(reduced,updatedAcc )
    }
  }

  def doIt2(s: Seq[Int], acc: Seq[Int]): Int = {

    val reduced = s.sliding(2).map(x => x.last - x.head).toSeq
    val updatedAcc = s.head +: acc
    if (reduced.toSet == Set(0)) {
      updatedAcc.reduce((a,b) => b - a)
    } else {
      doIt2(reduced, updatedAcc)
    }
  }
}
