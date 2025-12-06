package y2025

import scala.io.Source

object Day03 extends App{
  val data = Source.fromResource("2025/3.data").getLines().toSeq.map(_.toCharArray.map(_.asDigit))

  val part1 = data.map{ x =>
    val (first, loc) = x.init.zipWithIndex.maxBy(_._1)
    val second = x.splitAt(loc + 1)._2.zipWithIndex.maxBy(_._1)
    (first * 10) + second._1
  }.sum
  println(part1)


  implicit val o: Ordering[(Int, Int)] = (x: (Int, Int), y: (Int, Int)) => {
    if (x._1 == y._1) {
      (x._2 * -1).compare(y._2 * -1)
    } else {
      x._1.compare(y._1)
    }
  }

  val part2 = data.map(x =>findNum(x.toSeq, 12, 0, 100000000000L)).sum
  println(part2)

  def findNum(digits: Seq[Int], remaining:Int, acc : Long, mask: Long) : Long = {
    if(remaining == 0){
      acc
    } else if(remaining == 1){
      val d = digits.max
      val updatedAcc = acc + (d * mask)
      findNum(Seq.empty, remaining - 1, updatedAcc, mask / 10)
    }
    else{
      val set =  digits.take(digits.size - remaining + 1)
      val (d, loc) = digits.take(digits.size - remaining + 1).zipWithIndex.max(o)
      val sub = digits.splitAt(loc + 1)._2
      val updatedAcc = acc + (d * mask)
      findNum(sub, remaining - 1, updatedAcc, mask / 10)
    }
  }


}
