package y2023

import scala.io.Source

object Day02 extends App {

  val data = Source.fromResource("2023/2.data").getLines().toSeq

  val red = 12
  val green = 13
  val blue = 14

  val Game = raw"Game (\d+):(.+)".r

  val part1 = data.map{
    case Game(n, cubes) =>
      println(s"game: $n")
      val isGood = cubes.split("[;,]").forall{  x =>
        val parts = x.trim.split(" ")
        val count = parts.head.toInt
        val color = parts.last
        (color == "blue" && count <= blue) || (color == "red" && count <= red) || (color == "green" && count <= green)
      }
      if(isGood){
        n.toInt
      }else{
        0
      }
  }.sum

  println(part1)


  val part2 = data.map {
    case Game(n, cubes) =>
      println(s"game: $n")
      val x: (Int, Int, Int) = cubes.split("[;,]").foldLeft((0,0,0)) { case((r,g,b), x) =>
        val parts = x.trim.split(" ")
        val count = parts.head.toInt
        val color = parts.last
        if(color == "blue"){
          (r, g, Math.max(b, count))
        }else if(color == "green"){
          (r, Math.max(g, count), b)
        }else{
          (Math.max(r, count), g, b)
        }
      }
      x._1 * x._2 * x._3
  }.sum

  println(part2)



}
