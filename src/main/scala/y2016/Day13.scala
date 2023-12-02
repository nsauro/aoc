package y2016

object Day13 extends App{


  def isWall(x : Int, y: Int) : Boolean = {
    val z = ((x * x) + (3 * x) + (2 * x * y) + y + (y * y)) + 1350
    Integer.toBinaryString(z).count(_ == '1')  % 2 != 0
  }

  val r = (0 to 49).map{ y =>
    (0 to 49).map{ x =>
      if(x == 31 && y == 39){
        'X'
      }
      else if(isWall(x, y)){
        '#'
      }else{
        '.'
      }
    }
  }

  r.foreach(x => println(x.mkString("")))



  val answer = bfs(Set.empty, Seq(Node((1,1), 0)), (31,39))

  println(answer)

  println(bfsCount(Set.empty, Seq(Node((1,1), 0)), 50, 0))





  case class Node(coords: (Int, Int), dis : Int)

  def bfs(visited: Set[(Int, Int)], queue: Seq[Node], target : (Int, Int)) : Int = {
    val next = queue.head
    if(next.coords == target){
      next.dis
    }else{
      val neigbhors = getNeighbors(next.coords, visited, queue.tail)
      val nextQueue = queue.tail ++ neigbhors.map(Node(_, next.dis + 1))
      bfs(visited + next.coords, nextQueue, target)
    }

  }


  def bfsCount(visited: Set[(Int, Int)], queue: Seq[Node], maxDistance: Int, count : Int): Int = {
    if(queue.forall(_.dis > maxDistance)){
      count
    }else{

      val next = queue.head
      val neigbhors = getNeighbors(next.coords, visited, queue.tail)
      val nextQueue = queue.tail ++ neigbhors.map(Node(_, next.dis + 1))
      val nextCount = if(next.dis <= maxDistance) count + 1 else count
      bfsCount(visited + next.coords, nextQueue, maxDistance, nextCount)

    }

  }


  def getNeighbors(point : (Int, Int), visited: Set[(Int, Int)], queued: Seq[Node]) : Seq[(Int, Int)] = {
    val neighbors = Seq(
      (point._1 + 1, point._2),
      (point._1 - 1, point._2),
      (point._1, point._2 + 1),
      (point._1, point._2 - 1),
    )
    neighbors.filterNot(x => x._2 < 0 || x._1 < 0 || visited.contains(x) || queued.exists(_.coords == x) || isWall(x._1, x._2))
  }
}
