package y2023

import scala.io.Source

object Day01 extends App {

  val numbers = Seq(
    ("one" -> "1"),
    ("two" -> "2"),
    ("three" -> "3"),
    ("four" -> "4"),
    ("five" -> "5"),
    ("six" -> "6"),
    ("seven" -> "7"),
    ("eight" -> "8"),
    ("nine" -> "9")
  )


  val data = Source.fromResource("2023/1.data").getLines().toSeq

  /*val res = data.map{ x =>
    val digitsOnly = x.filter(_.isDigit)
    s"${digitsOnly.head}${digitsOnly.last}".toInt
  }.sum

  println(res)*/

  val res2 = data.map{ x =>
    val digitsOnly = toNumbers(x, Seq.empty)
    s"${digitsOnly.head}${digitsOnly.last}".toInt
  }.sum

  println(res2)

  def toNumbers(s : String, acc : Seq[String]) : Seq[String] = {
    if(s.isEmpty){
      acc
    }  else{
      if(s.head.isDigit){
        toNumbers(s.tail, acc :+ s.head.toString)
      }else{
        numbers.find(x => s.startsWith(x._1)) match {
          case Some((str, n)) => toNumbers(s.substring(str.length - 1), acc :+ n)
          case None => toNumbers(s.tail, acc)
        }
      }
    }
  }
}

