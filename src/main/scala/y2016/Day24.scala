package y2016

import scala.io.Source

object Day24 extends App{
    
    val data = Source.fromResource("2016/24.data").getLines().toArray

    for {
        y <- 0 until data.size
        x <- 0 until data.head.size
    } {
        if(data(y)(x) == '0'){
            println(s"$x - $y")
        }
    }


   /* def bfs(visited: Set[(Int, Int)], queue: Seq[(Int, Int)], remaining: Set[Int]) : Seq[Char] = {
    if(queue.head.x == target._1 && queue.head.y == target._2) {
      queue.head.path
    }else{
      val neighbors = getNeighbors(queue.head, visited, queue.tail)
      val nextQueue = queue.tail ++ neighbors
      bfs(visited + queue.head, nextQueue, target)
    }*/


  //}
  
}
