package y2016

import scala.io.Source

object Day24 extends App {

  val data =
    Source.fromResource("2016/24.data").getLines().toArray.map(_.toArray)

  val maxPoint = 7

  val points = (for {
    s <- 0 until maxPoint
    d <- s + 1 to maxPoint
  } yield {
    val starting = find(s.toString.head)
    val dis = bfs(Set.empty, Seq(Node(starting, 0)), d.toString.head)
    (s"$s$d" -> dis)
  }).toMap

  points.foreach(println)

  val res = (0 to maxPoint).permutations
    .filter(_.head == 0)
    .map { x =>
      x.sliding(2)
        .map(y =>
          points.getOrElse(s"${y.head}${y.last}", points(s"${y.last}${y.head}"))
        )
        .sum
    }
    .min

  println(res)

  val res2 = (0 to maxPoint).permutations
    .filter(_.head == 0)
    .map { x =>
      val all = x.toSeq :+ 0
      all
        .sliding(2)
        .map(y =>
          points.getOrElse(s"${y.head}${y.last}", points(s"${y.last}${y.head}"))
        )
        .sum
    }
    .min

  println(res2)

  def find(c: Char) = {
    println(s"finding $c")
    data.zipWithIndex.collectFirst {
      case (row, i) if (row.indexOf(c) != -1) => (i, row.indexOf(c))
    }.get
  }

  case class Node(coords: (Int, Int), dis: Int)

  def bfs(visited: Set[(Int, Int)], queue: Seq[Node], target: Char): Int = {
    val next = queue.head
    if (data(next.coords._1)(next.coords._2) == target) {
      next.dis
    } else {
      val neigbhors = getNeighbors(next.coords, visited, queue.tail)
      val nextQueue = queue.tail ++ neigbhors.map(Node(_, next.dis + 1))
      bfs(visited + next.coords, nextQueue, target)
    }
  }

  def getNeighbors(
      point: (Int, Int),
      visited: Set[(Int, Int)],
      queued: Seq[Node]
  ): Seq[(Int, Int)] = {
    val neighbors = Seq(
      (point._1 + 1, point._2),
      (point._1 - 1, point._2),
      (point._1, point._2 + 1),
      (point._1, point._2 - 1)
    )
    neighbors.filterNot(x =>
      x._2 < 0 || x._1 < 0 || visited.contains(x) || queued.exists(
        _.coords == x
      ) || data(x._1)(x._2) == '#'
    )
  }
}
