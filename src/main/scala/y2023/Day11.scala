package y2023

import scala.io.Source

object Day11 extends App {

  val data =
    Source.fromResource("2023/11.data").getLines().toArray.map(_.toCharArray)

  val expansionFactor = 1000000

  val expansionRows = data.zipWithIndex.collect {
    case (r, i) if (r.indexOf('#') == -1) => i
  }.toSet

  val expansionColumns = data.transpose.zipWithIndex.collect {
    case (c, i) if (c.indexOf('#') == -1) => i
  }.toSet

  val galaxies = for {
    r <- data.indices
    c <- data.head.indices if data(r)(c) == '#'
  } yield {
    (r, c)
  }

  val combos = galaxies.combinations(2).map(_.toSet.toSeq.sorted).toSet.toSeq

  val res = combos.map { x =>
    val manhattan =
      Math.abs(x.head._1 - x.last._1) + Math.abs(x.head._2 - x.last._2)
    val exp = expansionRows.count(r => isBetween(r, x.head._1, x.last._1)) +
      expansionColumns.count(c => isBetween(c, x.head._2, x.last._2))
    (manhattan + (exp * expansionFactor)).toLong - exp
  }.sum

  println(res)

  def isBetween(x: Int, a: Int, b: Int) = {
    (a <= x && x <= b) || (b <= x && x <= a)
  }

}
