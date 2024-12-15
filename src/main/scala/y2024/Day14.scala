package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App:

  val data = Source.fromResource("2024/14.data").getLines().toSeq

  println(data.size)

  val RobotR = """p=(.+),(.+) v=(.+),(.+)""".r

  val gridX = 101 //01234 5 678910
  val gridY = 103  //012 3 456

  val middleX = (gridX - 1) / 2
  val middleY = (gridY - 1) / 2

  //val times = 100

  val finalPos = data.map{
    case RobotR(x,y,mx,my) => computePos(x.toInt, y.toInt, mx.toInt, my.toInt, 100)
  }.sorted
  //finalPos.foreach(println)
  val part1Res = computeScore(finalPos, 0, 0, 0, 0)
  println(part1Res)




  val points = data.map {
    case RobotR(x, y, mx, my) => (x.toInt, y.toInt, mx.toInt, my.toInt)
  }.sorted

  part2(points, 1, Long.MaxValue)


  @tailrec
  def part2(points : Seq[(Int, Int, Int, Int)], times: Int, prevScore : Long) : Long = {


    val updated = points.map(x => computePos(x._1, x._2, x._3, x._4, times))
    val updatedScore = computeScore(updated, 0, 0, 0, 0)
    if updatedScore < prevScore then
      println(s"-------------------$times----------------------------------------------------------")
      printPoints(updated)
      Thread.sleep(3000)
    part2(points, times + 1, Math.min(updatedScore, prevScore))
  }


  def printPoints(points: Seq[(Int, Int)]) : Unit = {
    val grid = Array.fill(gridY)(Array.fill(gridX)('.'))
    points.foreach{ case(x,y) =>
      grid(y)(x) = 'x'
    }
    grid.foreach{ x=>
      println(x.mkString(""))
    }
  }



  def computePos(x: Int, y: Int, xMove: Int, yMove: Int, time: Int) :(Int, Int) = {
    val totalX = x + (xMove * time)
    val totalY = y + (yMove * time)

    val newX =
      if totalX < 0 then
        val maybe = gridX - Math.abs(totalX % gridX)
        if maybe == gridX then
          0
        else
          maybe

      else if totalX >= gridX then
         Math.abs(totalX % gridX)
      else
        totalX
    val newY =
      if totalY < 0 then
        val maybe = gridY - Math.abs(totalY % gridY)
        if maybe == gridY then
          0
        else
         maybe
      else if totalY >= gridY then
        Math.abs(totalY % gridY)
      else
        totalY
    (newX, newY)
  }

  def computeScore(pos :Seq[(Int, Int)], q1 : Long, q2: Long, q3: Long, q4: Long) : Long = {
    if pos.isEmpty then
      q1 * q2 * q3 * q4
    else
      if pos.head._1 < middleX && pos.head._2 < middleY then
        computeScore(pos.tail, q1 + 1, q2, q3, q4)
      else if pos.head._1 > middleX && pos.head._2 < middleY then
        computeScore(pos.tail, q1, q2 + 1, q3, q4)
      else if pos.head._1 < middleX && pos.head._2 > middleY then
        computeScore(pos.tail, q1, q2, q3 + 1, q4)
      else if pos.head._1 > middleX && pos.head._2 > middleY then
        computeScore(pos.tail, q1, q2, q3, q4 + 1)
      else
        computeScore(pos.tail, q1, q2, q3, q4)
  }
