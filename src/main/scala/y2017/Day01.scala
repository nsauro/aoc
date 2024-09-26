package y2017

import scala.io.Source

object Day01 extends App:

  val data =
    Source.fromResource("2017/1.data").getLines().toSeq.mkString("").toCharArray

  val x: Seq[Array[Char]] =
    data.sliding(2).toSeq ++ Seq(Array(data.last, data.head))
  val answer = x.foldLeft(0) {
    case (acc, Array(f, l, _*)) if f == l => acc + f.asDigit
    case (acc, _)                         => acc
  }

  println(answer)

  val halfway = data.size / 2
  val y = data.zipWithIndex.foldLeft(0) { case (acc, (c, i)) =>
    val newI = if ((i + halfway) < data.size) {
      i + halfway
    } else {
      (i + halfway) - data.size
    }
    if (data(newI) == c) {
      acc + c.asDigit
    } else {
      acc
    }
  }
  println(y)
