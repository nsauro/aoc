package y2016

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  val data = Source.fromResource("2016/2.data").getLines().toSeq

  val keypad = Array(
    Array("", "", "1", "", ""),
    Array("", "2", "3", "4", ""),
    Array("5", "6", "7", "8", "9"),
    Array("", "A", "B", "C", ""),
    Array("", "", "D", "", "")
  )

  println(getKeys((2, 0), data, ""))

  @tailrec
  def getKeys(curLoc: (Int, Int), ins: Seq[String], acc: String): String = {
    if (ins.isEmpty) {
      acc
    } else {
      val newLoc = ins.head.foldLeft(curLoc) {
        case (acc, 'L') if acc._2 > 0 && keypad(acc._1)(acc._2 - 1) != "" =>
          acc.copy(_2 = acc._2 - 1)
        case (acc, 'R') if acc._2 < 4 && keypad(acc._1)(acc._2 + 1) != "" =>
          acc.copy(_2 = acc._2 + 1)
        case (acc, 'U') if acc._1 > 0 && keypad(acc._1 - 1)(acc._2) != "" =>
          acc.copy(_1 = acc._1 - 1)
        case (acc, 'D') if acc._1 < 4 && keypad(acc._1 + 1)(acc._2) != "" =>
          acc.copy(_1 = acc._1 + 1)
        case (acc, x) => acc
      }
      getKeys(newLoc, ins.tail, acc + keypad(newLoc._1)(newLoc._2))
    }
  }

}
