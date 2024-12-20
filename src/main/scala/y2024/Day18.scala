package y2024

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day18 extends App:

  val gridSize = 71
  val target = (gridSize - 1, gridSize - 1)
  val allBytes = Source.fromResource("2024/18.data").getLines().map{ x =>
    val parts = x.split(",")
    (parts.head.toInt, parts.last.toInt)
  }.toArray

  println(allBytes.length)
  println(bfs(mutable.Map((0,0)->0), ListBuffer((0,0)), allBytes.take(1024).toSet))

  println(part2(2700))

  def part2(i : Int) : (Int, Int) = {
    println(i)
    val res = bfs(mutable.Map((0,0)->0), ListBuffer((0,0)), allBytes.take(i).toSet)
    if res.isEmpty then
      allBytes(i - 1)
    else
     part2(i + 1)
  }

  def bfs(
           visited: mutable.Map[(Int, Int), Int],
           queue: ListBuffer[(Int, Int)],
           bytes: Set[(Int, Int)]
         ): Option[Int] = {
    if queue.isEmpty then
      visited.get(target)
    else
      val next = queue.head
      val neighbors = getNeighbors(next, visited.keys.toSet, queue.tail, bytes)
      neighbors.foreach{ x=>
        visited.put(x, visited(next) + 1)
      }
      val nextQueue = queue.tail ++ neighbors
      bfs(visited, nextQueue, bytes)

  }

  def getNeighbors(
                    point: (Int, Int),
                    visited: Set[(Int, Int)],
                    queued: ListBuffer[(Int, Int)],
                    bytes : Set[(Int, Int)]
                  ): ListBuffer[(Int, Int)] = {
    val neighbors = ListBuffer(
      (point._1 + 1, point._2),
      (point._1 - 1, point._2),
      (point._1, point._2 + 1),
      (point._1, point._2 - 1)
    )
    neighbors.filterNot(x =>
      x._1 < 0 || x._1 >= gridSize || x._2 < 0 || x._2 >= gridSize
      || visited.contains(x) || queued.contains(x) || bytes.contains(x)
      )
  }





