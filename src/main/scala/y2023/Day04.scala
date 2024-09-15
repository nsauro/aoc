package y2023

import scala.io.Source

object Day04 extends App {

  val data = Source.fromResource("2023/4.data").getLines().toSeq

  val Game = raw"""Card(.+): (.+) \| (.+)""".r

  val sum = data.collect { case Game(_, w, y) =>
    val winning =
      w.trim.split(" ").map(_.trim).filter(_ != "").map(_.toInt).toSet
    val yours = y.trim.split(" ").map(_.trim).filter(_ != "").map(_.toInt)
    yours.foldLeft(0) { case (acc, x) =>
      if (winning.contains(x)) {
        if (acc == 0) 1 else acc * 2
      } else {
        acc
      }
    }
  }.sum

  println(sum)

  // -----part 2

  // just easier this way
  val copies = new scala.collection.mutable.HashMap[Int, Int]

  val totalCopies = data.foldLeft(copies) {
    case (acc, Game(i, w, y)) => {
      val g = i.trim.toInt
      val winning =
        w.trim.split(" ").map(_.trim).filter(_ != "").map(_.toInt).toSet
      val yours = y.trim.split(" ").map(_.trim).filter(_ != "").map(_.toInt)
      val winningCount = yours.count(winning.contains)
      val inc = acc.getOrElse(g, 0) + 1
      ((g + 1 to g + winningCount)).foreach { x =>
        acc.put(x, acc.getOrElse(x, 0) + inc)
      }
      acc
    }
  }
  val totalCards = data.length + totalCopies.values.sum
  println(totalCards)

}
