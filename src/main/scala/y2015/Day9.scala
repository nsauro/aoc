package y2015

import scala.io.Source

object Day9 extends App {

  val Distance = raw"""(.+) to (.+) = (\d+)""".r

  val data = Source.fromResource("2015/9.data").getLines().toSeq

  val distances: Map[Set[String], Int] =
    data.foldLeft(Map.empty[Set[String], Int]) { case (acc, str) =>
      str match {
        case Distance(from, to, d) => acc + (Set(from, to) -> d.toInt)
      }
    }

  val allLocations = distances.keys.toSet.flatten.toSeq

  val allDistances = allLocations.permutations.map { x =>
    x.sliding(2).foldLeft(0) { case (acc, pair) =>
      acc + distances(pair.toSet)
    }
  }.toSeq

  println(allDistances.min)
  println(allDistances.max)
}
