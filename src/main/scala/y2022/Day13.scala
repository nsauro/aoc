package y2022

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day13 extends App {

  val data = Source.fromResource("2022/Day13").getLines().toSeq

  val parsed = data.collect {
    case s if s.nonEmpty => parse(s)
  }

  val pairs =
    parsed.grouped(2).zipWithIndex.map(x => x.copy(_2 = x._2 + 1)).toSeq

  val res = pairs.collect {
    case (items, i) if (compare(items.head, items.last) < 0) => i
  }.sum

  println(res)

  val two = parse("[[2]]")
  val six = parse("[[6]]")

  val sorted = (parsed ++ Seq(two, six)).sortWith(sort)
  sorted.foreach(println)
  val twoIndex = sorted.indexOf(two) + 1
  val sixIndex = sorted.indexOf(six) + 1
  println(twoIndex * sixIndex)

  def parse(s: String): Item = {
    s.split(",").flatMap(_.toCharArray)

    val all = s.foldLeft((ListBuffer.empty[Items], false)) {
      case ((acc, lastWasDigit), c) =>
        c match {
          case '[' => {
            val b = Items(ListBuffer.empty[Item])
            if (acc.nonEmpty) {
              acc.last.i.addOne(b)
            }
            (acc.addOne(b), false)
          }
          case ']' => if (acc.size > 1) (acc.init, false) else (acc, false)
          case ',' => (acc, false)
          case d => {
            if (lastWasDigit) {
              val x = acc.last.i.last.toString + d
              acc.last.i.remove(acc.last.i.length - 1)
              acc.last.i.addOne(Digit(Integer.valueOf(x)))
              (acc, true)

            } else {
              acc.last.i.addOne(Digit(d.asDigit))
              (acc, true)
            }
          }
        }

    }
    all._1.head
  }

  def compare(a: Item, b: Item): Int = {
    (a, b) match {
      case (Digit(x), Digit(y)) => x.compareTo(y)
      case (x: Digit, y: Items) => compare(Items(ListBuffer(x)), y)
      case (x: Items, y: Digit) => compare(x, Items(ListBuffer(y)))
      case (Items(x), Items(y)) => compLists(x.toSeq, y.toSeq)
    }

  }
  def compLists(x: Seq[Item], y: Seq[Item]): Int = {
    if (x.isEmpty && y.isEmpty) {
      0
    } else if (x.isEmpty) {
      -1
    } else if (y.isEmpty) {
      1
    } else {
      val res = compare(x.head, y.head)
      if (res == 0) {
        compLists(x.tail, y.tail)
      } else {
        res
      }
    }

  }

  def sort(a: Item, b: Item): Boolean = {
    compare(a, b) < 0
  }

  sealed trait Item

  case class Digit(i: Int) extends Item {
    override def toString: String = i.toString
  }

  case class Items(i: ListBuffer[Item]) extends Item {
    override def toString: String = s"[${i.mkString(",")}]"
  }

}
