package y2023

import scala.collection.mutable
import scala.io.Source

object Day21 extends App{

  val raw = Source.fromResource("2023/21.data").getLines().toArray.map(_.toArray)

  val starting = raw.zipWithIndex.collectFirst{
    case(row, r) if row.contains('S') => (r, row.indexOf('S'))
  }.get
  println(starting)
  println(raw.length)
  println(raw.head.length)

  println(step(64, mutable.HashSet(starting)))
  def step(stepsLeft : Int, locations: mutable.HashSet[(Int, Int)]) : Int = {
    if(stepsLeft == 0){
      locations.size
    }else{
      val newLocs = locations.foldLeft(mutable.HashSet.empty[(Int, Int)]){ case(acc, p) =>
        acc.addAll(getNeighbors(p._1, p._2))
      }
      step(stepsLeft - 1, newLocs)
    }
  }

  def getNeighbors(r : Int, c : Int) : Seq[(Int, Int)] = {
    Seq(
      (r + 1, c),
      (r - 1, c),
      (r, c + 1),
      (r, c - 1)
    ).filterNot(x =>
      x._1 < 0 ||
        x._1 == raw.length ||
        x._2 < 0 ||
        x._2 == raw.head.length ||
        raw(x._1)(x._2) == '#'
    )
  }

}
