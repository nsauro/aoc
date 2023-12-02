package y2022

import scala.io.Source

object Day12 extends App{

  val data: Array[Array[Char]] = Source.fromResource("2022/Day12").getLines().toArray.map(_.toCharArray)
  val cache = scala.collection.mutable.Map.empty[(Int, Int), Int]

  println(compute2(20,0, Seq.empty))





  def compute2(row : Int, column : Int, visitedNodes : Seq[(Int, Int)]) : Int = {
    if(data(row)(column) == 'E'){
      0
    }else{
      val neighbors = Seq((row + 1, column), (row - 1, column), (row, column +1), (row, column -1)).filter { c =>
        !visitedNodes.contains(c) &&
          c._1 >= 0 &&
          c._1 < data.length &&
          c._2 >= 0 &&
          c._2 < data.head.length &&
          (getValue(data(c._1)(c._2)) - getValue(data(row)(column)) == 1 || getValue(data(c._1)(c._2)) - getValue(data(row)(column)) == 0)
      }
      if(neighbors.isEmpty){
        cache.put((row -> column), -1)
        -1
      }else{
        val updated = neighbors.map{ x =>
          cache.getOrElse(x, compute2(x._1, x._2, visitedNodes :+ (row -> column)))
        }.filter(_ != -1).map(_ + 1)
        val min = if(updated.isEmpty){
          -1
        }  else{
          updated.min
        }
        cache.put((row->column), min)
        min
      }
    }
  }


  /*def compute(row : Int, column : Int, visitedNodes : Seq[(Int, Int)], curSteps : Int)  : Option[Int] = {

    println(curSteps)

    if(data(row)(column) == 'E'){
      println(s"nailed it!: $curSteps : $visitedNodes")
      Some(curSteps)
    }else{
      val neighbors = Seq((row + 1, column), (row - 1, column), (row, column +1), (row, column -1)).filter{ c =>
        !visitedNodes.contains(c) &&
          c._1 >= 0 &&
          c._1 < data.length &&
          c._2 >= 0 &&
          c._2 < data.head.length &&
          !duds.contains(c) &&
          (getValue(data(c._1)(c._2)) - getValue(data(row)(column)) == 1 || getValue(data(c._1)(c._2)) - getValue(data(row)(column)) == 0)
      }
      if(neighbors.isEmpty){
        println(s"bomb: $row - $column $curSteps -- ${duds.size}")
        duds.add(row -> column)
        None
      }else{
        val updatedNodes = visitedNodes :+ (row -> column)
        val paths = neighbors.flatMap( n => compute(n._1, n._2, updatedNodes, curSteps + 1))
        if(paths.isEmpty){
          duds.add(row -> column)
          None
        }else{
          Some(paths.min)
        }
      }

    }

  }*/
  def getValue(c : Char) = {
    c match {
      case 'S' => 'a'.toInt
      case 'E' => 'z'.toInt
      case other => other.toInt
    }
  }





}
