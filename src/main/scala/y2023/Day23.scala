package y2023

import scala.io.Source

object Day23 extends App {
  val data = Source.fromResource("2023/23.data").getLines().toArray.map(_.toArray)

  println(data.map(_.count(_ != '#')).sum)

  val start = (0, data.head.indexOf('.'))
  val target = (data.length - 1, data.last.indexOf('.'))

  val res = dfs(Seq(start), Seq.empty, 0)
  println(res - 1)


  def dfs(path: Seq[(Int, Int)], queued: Seq[(Int, Int)], max : Int) : Int = {
    if(path.last == target){
      val res = Math.max(max, path.length)
      println(res)
      res
    }else{
      val neighbors = getNeighbors(path.last, path, queued)
      if(neighbors.isEmpty){
        max
      }else if(neighbors.size == 1){
        dfs(path :+ neighbors.head, queued, max)
      }else{
       // println(s"splitting ${neighbors} -- ${path.length} -- ${path.last}")
        val r = neighbors.foldLeft (max){case(max, next) =>
          Math.max(max, dfs(path :+ next, neighbors ++ queued, max))
        }
       // println(s"finished with split ${neighbors} -- ${path.length} -- ${path.last}")
        r
      }
    }
  }


  def getNeighbors(cur: (Int, Int), visited: Seq[(Int, Int)], queued: Seq[(Int, Int)]) : Seq[(Int, Int)] = {
    (data(cur._1)(cur._2) match{
      //case '>' => Seq(cur.copy(_2 = cur._2 + 1))
      //case '<' => Seq(cur.copy(_2 = cur._2 - 1))
      //case '^' => Seq(cur.copy(_1 = cur._1 - 1))
      //case 'v' => Seq(cur.copy(_1 = cur._1 + 1))
      case _ => {
        Seq(
          cur.copy(_1 = cur._1 + 1),
          cur.copy(_1 = cur._1 - 1),
          cur.copy(_2 = cur._2 - 1),
          cur.copy(_2 = cur._2 + 1)
        )
      }
    }).filterNot { x =>
      x._1 < 0 ||
        x._1 == data.length ||
        x._2 < 0 ||
        x._2 == data.head.length ||
        data(x._1)(x._2) == '#' ||
        visited.contains(x) ||
        queued.contains(x)
    }
  }


}
