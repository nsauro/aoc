package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day20 extends App:

  val grid = Source.fromResource("2024/20.data").getLines().map(_.toCharArray).toArray
  val start = findChar(0, 0, 'S')
  val end = findChar(0, 0, 'E')


  val path = getPath(Seq(start))
  println(path.length)
  val pathWithDistances = path.zipWithIndex

  println(countShortCuts(Vector.from(pathWithDistances), 20, 100))

  def countShortCuts(p : Vector[((Int, Int), Int)], maxCut: Int, targetSave: Int) : Int = {
    var count: Int = 0
    for {
      i <- p.indices
      j <- 1 until p.length
    } {
      val (p1, p1Dis) = p(i)
      val (p2, p2Dis) = p(j)
      val mdis = manDis(p1, p2)
      val totalSave = p2Dis - p1Dis - mdis
      if mdis <= maxCut && totalSave > 0 &&  (totalSave < p2Dis - p1Dis) && totalSave >= targetSave  then
        count += 1
    }
    count
  }

  def manDis(p1 : (Int, Int), p2: (Int, Int)) : Int = {
    Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)
  }

  def getPath(path: Seq[(Int, Int)]) : Seq[(Int, Int)] = {
    if path.last == end then
      path
    else
      val neighbors = getNeighbors(path.last, path)
      if neighbors.length == 1 then
        getPath(path :+ neighbors.head)
      else if neighbors.isEmpty then
        path
      else
        neighbors.foldLeft(path){ case (acc, n) =>
          if acc.last != end then
            getPath(path :+ n)
          else
            acc
        }

  }

  def getNeighbors(
                    point: (Int, Int),
                    visited: Seq[(Int, Int)]
                  ): Seq[(Int, Int)] = {
    val neighbors = Seq(
      (point._1 + 1, point._2),
      (point._1 - 1, point._2),
      (point._1, point._2 + 1),
      (point._1, point._2 - 1)
    )
    neighbors.filterNot(x =>
      x._1 < 0 || x._1 >= grid.head.length || x._2 < 0 || x._2 >= grid.length ||
        grid(x._2)(x._1) == '#' ||
      visited.contains(x))
  }


  @tailrec
  def findChar(x: Int, y: Int, char: Char): (Int, Int) = {
    if grid(y)(x) == char then
      (x, y)
    else if y + 1 == grid.head.length then
      findChar(x + 1, 0, char)
    else
      findChar(x, y + 1, char)
  }