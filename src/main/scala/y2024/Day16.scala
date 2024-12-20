package y2024

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day16 extends App:

  type Location = ((Int, Int), Char)

  val grid = Source.fromResource("2024/16.data").getLines().map(_.toCharArray).toArray
  val start = findChar(0, 0, 'S')
  val end = findChar(0, 0, 'E')

  case class Node(x: Int, y: Int, dis: Long, direction: Char)

  implicit val o: Ordering[Node] = Ordering.by(-_.dis)

  val queue = mutable.PriorityQueue.empty
  val dis = mutable.Map.empty[(Int, Int), Long]

  grid.zipWithIndex.foreach{ case (row, y) =>
    row.zipWithIndex.foreach{ case (c, x) =>
      if c != '#' then
        if x == start._1 && y == start._2 then
          queue.enqueue(Node(x, y, 0, 'R'))
          dis.put((x,y), 0)
        else
          queue.enqueue(Node(x, y, Long.MaxValue, '-'))
          dis.put((x, y), Long.MaxValue)
    }
  }

  /*val part1Res = djk(queue, dis)
  println(part1Res(end))*/

  val part2Res = djk2(queue, dis)
  println(part2Res)
  println(countNodes(ListBuffer(end), part2Res, mutable.LinkedHashSet.empty).size)

  def djk(queue: mutable.PriorityQueue[Node], finished: mutable.Map[(Int, Int), Long]) : mutable.Map[(Int, Int), Long] = {

    while(queue.nonEmpty){
      val next = queue.dequeue()
      if finished(next.x, next.y) == next.dis then
        //check right
        val neighbors = getNextNodes(next)
        neighbors.foreach{ neighbor =>
          val k = (neighbor.x, neighbor.y)
          if finished(k) > neighbor.dis then
            finished(k) = neighbor.dis
          queue.enqueue(neighbor)
        }
    }
    finished
  }

  def djk2(queue: mutable.PriorityQueue[Node], finished: mutable.Map[(Int, Int), Long]): mutable.Map[(Int, Int), ListBuffer[(Int, Int, Char)]] = {

    val paths = mutable.Map.empty[(Int, Int), ListBuffer[(Int, Int, Char)]]
    while (queue.nonEmpty) {
      val next = queue.dequeue()
      println(next)
      if finished(next.x, next.y) == next.dis then
        val neighbors = getNextNodes(next)
        neighbors.foreach { neighbor =>
          println(s"processing neighbor: $neighbor")
          val k = (neighbor.x, neighbor.y)
          if finished(k) > neighbor.dis then
            finished(k) = neighbor.dis
            paths((neighbor.x, neighbor.y)) = ListBuffer((next.x, next.y, next.direction))
          else if finished(k) == neighbor.dis then
              paths((neighbor.x, neighbor.y)).addOne((next.x, next.y, next.direction))
          queue.enqueue(neighbor)
        }
    }
    paths
  }


  def getNextNodes(node: Node) : Seq[Node] = {
    Seq(
      tryMove(node, x => x.copy(_1 = x._1 + 1), 'R'),
      tryMove(node, x => x.copy(_1 = x._1 - 1), 'L'),
      tryMove(node, x => x.copy(_2 = x._2 + 1), 'D'),
      tryMove(node, x => x.copy(_2 = x._2 - 1), 'U')
    ).flatten
  }


  def tryMove(prev: Node, update: ((Int, Int)) => (Int, Int), dir: Char ) : Option[Node] = {
    val updated = update((prev._1, prev._2))
    Option.when(isValid(updated)){
      val dis = prev.dis + 1 + (if prev.direction != dir then 1000 else 0)
      Node(updated._1, updated._2, dis, dir)
    }
  }


  def countNodes(keys: ListBuffer[(Int, Int)], paths: mutable.Map[(Int, Int), ListBuffer[(Int, Int, Char)]], nodes: mutable.LinkedHashSet[(Int, Int)]) :  mutable.LinkedHashSet[(Int, Int)] = {
    if keys.isEmpty then
      nodes
    else
      keys.foreach{ k =>
        println(k)
        nodes.add(k)
        countNodes(paths.getOrElse(k, ListBuffer.empty).map(x => ((x._1, x._2))), paths, nodes)
      }
      nodes
  }

  def isValid(loc: (Int,Int)) : Boolean = {
    loc._1 >= 0 &&
      loc._1 < grid.head.length
    && loc._2 >= 0 &&
      loc._2 < grid.length &&
      grid(loc._2)(loc._1) != '#'
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
