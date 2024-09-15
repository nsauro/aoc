package y2015

import scala.io.Source

object Day13 extends App {

  val Happiness =
    raw"""(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+).""".r

  val data = Source.fromResource("2015/13.data").getLines()

  val happinessMap = data.foldLeft(Map.empty[(String, String), Int]) {
    case (acc, item) =>
      item match {
        case Happiness(from, action, value, to) => {
          val v = if (action == "gain") {
            value.toInt
          } else {
            value.toInt * -1
          }

          acc + ((from, to) -> v)
        }
      }
  }

  val people = happinessMap.keys.flatMap(x => Seq(x._1, x._2)).toSeq.distinct

  println(people)

  val allValues = people.permutations.map { x =>
    val circle = x :+ x.head
    circle
      .sliding(2)
      .map(x => happinessMap((x.head, x.last)) + happinessMap(x.last, x.head))
      .sum
  }

  println(allValues.max)

}
