package y2016


import java.security.MessageDigest

object Day17 extends App{


  val code =  "vwbaicqe"
  val openDoors = Set('b', 'c', 'd', 'e','f')

  val md5Hasher = MessageDigest.getInstance("MD5")
  val format = java.util.HexFormat.of()

  case class Location(x: Int, y:Int, path: Seq[Char])

  println(bfs(Set.empty, Seq(Location(0,0,Seq.empty)), (3,3)).mkString(""))
  println(bfsLongest(Set.empty, Seq(Location(0,0,Seq.empty)), (3,3), Seq.empty))


  def bfs(visited: Set[Location], queue: Seq[Location], target: (Int, Int)) : Seq[Char] = {
    if(queue.head.x == target._1 && queue.head.y == target._2) {
      queue.head.path
    }else{
      val neighbors = getNeighbors(queue.head, visited, queue.tail)
      val nextQueue = queue.tail ++ neighbors
      bfs(visited + queue.head, nextQueue, target)
    }


  }


  def bfsLongest(visited: Set[Location], queue: Seq[Location], target: (Int, Int), matches: Seq[String]): Int = {
    if(queue.isEmpty){
      matches.map(_.length).max
    }
    else if (queue.head.x == target._1 && queue.head.y == target._2) {
      bfsLongest(visited, queue.tail, target, matches :+ queue.head.path.mkString(""))
    } else {
      val neighbors = getNeighbors(queue.head, visited, queue.tail)
      val nextQueue = queue.tail ++ neighbors
      bfsLongest(visited + queue.head, nextQueue, target, matches)
    }


  }



  def getNeighbors(location : Location, visited: Set[Location], queued: Seq[Location]) : Seq[Location] = {
    val str = code + location.path.mkString("")
    val res = md5Hasher.digest(str.getBytes("UTF-8"))
    val hash = format.formatHex(res)

    val rawNeighbors = Seq(
      (hash(0), location.copy(y = location.y - 1, path = location.path :+ 'U')),  //up
      (hash(1), location.copy(y = location.y + 1, path = location.path :+ 'D')),  //up:
      (hash(2), location.copy(x = location.x - 1, path = location.path :+ 'L')),  //left:
      (hash(3), location.copy(x = location.x + 1, path = location.path :+ 'R'))  //left:
    )

    rawNeighbors.filter{
      case (d, loc) => {
        openDoors.contains(d) &&
          !visited.contains(loc) &&
          loc.x >= 0 && loc.x <= 3 &&
          loc.y >= 0 && loc.y <= 3 &&
          !queued.contains(loc)
      }
    }.map(_._2)


  }

}
