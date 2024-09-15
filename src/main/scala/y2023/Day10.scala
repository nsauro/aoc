package y2023

import scala.io.Source

object Day10 extends App {

  val data = Source.fromResource("2023/10.data").getLines().toArray
  val pipeChars = Set('S', '-', '|', '7', 'L', 'F', 'J', '#')

  val start = data.zipWithIndex.collectFirst {
    case (row, r) if (row.contains('S')) => (r, row.indexOf('S'))
  }.get

  val res = walk(start.copy(_1 = start._1 + 1), start, Seq.empty).toSet
  println(res.size / 2)

  def walk(
      cur: (Int, Int),
      from: (Int, Int),
      acc: Seq[(Int, Int)]
  ): Seq[(Int, Int)] = {
    data(cur._1)(cur._2) match {
      case '|' =>
        val next = getNext(
          Seq(cur.copy(_1 = cur._1 + 1), cur.copy(_1 = cur._1 - 1)),
          from
        )
        walk(next, cur, acc :+ cur)
      case '-' =>
        val next = getNext(
          Seq(cur.copy(_2 = cur._2 + 1), cur.copy(_2 = cur._2 - 1)),
          from
        )
        walk(next, cur, acc :+ cur)
      case 'L' =>
        val next = getNext(
          Seq(cur.copy(_1 = cur._1 - 1), cur.copy(_2 = cur._2 + 1)),
          from
        )
        walk(next, cur, acc :+ cur)
      case 'J' =>
        val next = getNext(
          Seq(cur.copy(_1 = cur._1 - 1), cur.copy(_2 = cur._2 - 1)),
          from
        )
        walk(next, cur, acc :+ cur)
      case '7' =>
        val next = getNext(
          Seq(cur.copy(_1 = cur._1 + 1), cur.copy(_2 = cur._2 - 1)),
          from
        )
        walk(next, cur, acc :+ cur)
      case 'F' =>
        val next = getNext(
          Seq(cur.copy(_1 = cur._1 + 1), cur.copy(_2 = cur._2 + 1)),
          from
        )
        walk(next, cur, acc :+ cur)
      case '.' =>
        acc :+ cur
      case 'S' => acc :+ cur
    }
  }

  def getNext(options: Seq[(Int, Int)], prev: (Int, Int)) = {
    options.filter { x =>
      x != prev && x._1 >= 0 && x._1 < data.length && x._2 >= 0 && x._2 < data.head.length
    }.head
  }

//-------- part 2 stuff

  // 2x grid
  val newGrid =
    Array.fill(data.length * 2 + 2)(Array.fill(data.head.length * 2 + 2)('*'))

  for {
    r <- newGrid.indices
    c <- newGrid.head.indices
  } {
    if (isBoundary(r, c)) {
      newGrid(r)(c) = '.'
    } else if (newGrid(r)(c) == '*') { // not seen
      if (r % 2 == 1) { // dilation row
        if (c % 2 == 1) { // dilation column

          val originalCoords = (r / 2, c / 2)
          if (res.contains(originalCoords)) {
            newGrid(r)(c) =
              data(originalCoords._1)(originalCoords._2) // copy to new location
            data(originalCoords._1)(originalCoords._2) match {
              case '-' | 'L' | 'S' =>
                newGrid(r)(c + 1) = '#'
              case '|' | '7' =>
                newGrid(r + 1)(c) = '#'
              case 'F' =>
                newGrid(r)(c + 1) = '#'
                newGrid(r + 1)(c) = '#'
              case 'J' => // nothing
            }
          } else {
            newGrid(r)(c) = '+'
          }

        } else {
          newGrid(r)(c) = '.'
        } // expansion column
      } else { // expansion row
        newGrid(r)(c) = '.'
      }
    }
  }

  mergeFill(Set((0, 0)), Set.empty)
  println(newGrid.map(_.count(_ == '+')).sum)
  def isBoundary(r: Int, c: Int) = {
    r == 0 || r == newGrid.length - 1 || c == 0 || c == newGrid.head.length - 1
  }

  def mergeFill(queue: Set[(Int, Int)], visited: Set[(Int, Int)]): Unit = {
    if (queue.nonEmpty) {
      newGrid(queue.head._1)(queue.head._2) = '*'
      val neighbors = getNeighbors(queue.head, visited)
      mergeFill(queue.tail ++ neighbors, visited + queue.head)
    }

  }

  def getNeighbors(
      point: (Int, Int),
      visited: Set[(Int, Int)]
  ): Set[(Int, Int)] = {
    val neighbors = Set(
      (point._1 + 1, point._2),
      (point._1 - 1, point._2),
      (point._1, point._2 + 1),
      (point._1, point._2 - 1)
    )
    neighbors.filterNot(x =>
      x._2 < 0 ||
        x._1 < 0 ||
        visited.contains(x) ||
        x._1 == newGrid.length ||
        x._2 == newGrid.head.length ||
        pipeChars.contains(newGrid(x._1)(x._2))
    )
  }

  /*def bfs(visited: Set[(Int, Int)], queue: Seq[Node], target: (Int, Int)): Int = {
    val next = queue.head
    if (next.coords == target) {
      next.dis
    } else {
      val neigbhors = getNeighbors(next.coords, visited, queue.tail)
      val nextQueue = queue.tail ++ neigbhors.map(Node(_, next.dis + 1))
      bfs(visited + next.coords, nextQueue, target)
    }

  }


   */

}
