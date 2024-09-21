package y2017

import scala.io.Source

object Day04 extends App:
  val data =
    Source.fromResource("2017/4.data").getLines().toSeq

  val res = data.count{ row =>
    val words = row.split("\\s+").map(_.trim).filter(_ != "")
    val deduped = words.toSet
    words.length == deduped.size
  }

  println(res)

  val res2 = data.count{ row =>
    val words = row.split("\\s+").map(_.trim).filter(_ != "")
    println(words.mkString("Array(", ", ", ")"))
    val hasAnagram = words.combinations(2).collectFirst{
      case Anagrams() => true
    }
    println(hasAnagram)
    hasAnagram.isEmpty
  }

  println(res2)


  object Anagrams {
    def unapply(s: Array[String]): Boolean = {
      println(s"checking:${s.mkString(", ")}")
      if (s.length != 2 || s.head.length != s.last.length) {
        false
      } else {
        val m = s.head.foldLeft(Map.empty[Char, Int]) { case (acc, c) =>
          if (acc.contains(c)) {
            acc.updated(c, acc(c) + 1)
          } else {
            acc.updated(c,  1)
          }
        }
        val remaining = s.last.foldLeft(m) { case (acc, c) =>
          if (acc.contains(c) && acc(c) > 1) {
            acc.updated(c,  acc(c) - 1)
          } else if (acc.contains(c)) {
            acc.removed(c)
          } else {
            acc
          }
        }
        println(remaining)
        remaining.isEmpty
      }
    }
  }



