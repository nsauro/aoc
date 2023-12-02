package y2015

import scala.io.Source

object Day16 extends App{

  val knownProperties = Map(
    "children" -> 3,
    "samoyeds" -> 2,
    "akitas" -> 0,
    "vizslas" -> 0,
    "cars" -> 2,
    "perfumes" -> 1
  )

  val greaterThanProperties = Map(
    "cats" -> 7,
    "trees" -> 3,
  )

  val lessThanProperties = Map(
    "pomeranians" -> 3,
    "goldfish" -> 5,
  )

  val data = Source.fromResource("2015/16.data").getLines()

  val SueFragmentData = raw"""Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)""".r

  case class SueFragment(id : Int, properties : Map[String, Int])

  val sueFragements = data.map{
      case SueFragmentData(id, p1Name, p1Value, p2Name, p2Value, p3Name, p3Value) => {
      SueFragment(id.toInt, Map(p1Name -> p1Value.toInt, p2Name -> p2Value.toInt, p3Name -> p3Value.toInt))
    }
    }.toSeq

  println(sueFragements)

  val filteredOut = sueFragements.filterNot{ x =>

    x.properties.exists(x => knownProperties.contains(x._1) && knownProperties(x._1) != x._2) ||
      x.properties.exists(x => greaterThanProperties.contains(x._1) && greaterThanProperties(x._1) >= x._2) ||
      x.properties.exists(x => lessThanProperties.contains(x._1) && lessThanProperties(x._1) <= x._2)
  }

  println(filteredOut.size)
  filteredOut.foreach(println)





}
