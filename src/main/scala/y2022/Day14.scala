package y2022

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App{


  val data = Source.fromResource("2022/Day14").getLines().toSeq

  val rocks = data.flatMap(parseRock)
  val leftMost = rocks.minBy(_._1)._1
  val rightMost = rocks.maxBy(_._1)._1
  val bottomMost = rocks.maxBy(_._2)._2
  val realBottomMost = bottomMost + 2

  println(leftMost)
  println(rightMost)
  println(bottomMost)


  val sand = scala.collection.mutable.Set.empty[(Int, Int)]


  def parseRock(s : String) : Set[(Int, Int)] = {
    s.split("->").map{ x =>
      val parts = x.trim.split(",")
      (parts.head.toInt, parts.last.toInt)
    }.sliding(2).flatMap{  pair =>
      for {
        x <- Math.min(pair.head._1, pair.last._1) to Math.max(pair.head._1, pair.last._1)
        y <- Math.min(pair.head._2, pair.last._2) to Math.max(pair.head._2, pair.last._2)
      } yield{
        (x,y)
      }
    }.toSet
  }

  printGrid()

  //addUntilBust(0)
  //println(sand.size)

  addUntilBust2()
  println(sand.size)


  @tailrec
  def addUntilBust(added : Int) : Unit = {

    if(!addSand((500, 0))){
      printGrid()
    }else{
      //println(s"adding more sand: ${updated.size} -- ${sand.size}")
      if(added % 500 == 0){
        println(s"$added sand has fallen")
      }
      addUntilBust(added + 1)
    }
  }

  @tailrec
  def addSand(point : (Int, Int)) : Boolean = {
    //check below first
    if(point._1 < leftMost || point._1 > rightMost || point._2 > bottomMost){ //zee abyss
      println(s"zee abyss: $point")
      false
    }else{
      val below = (point._1, point._2 + 1)
      val diagonalLeft = (below._1 - 1, below._2)
      val diagonalRight = (below._1 + 1, below._2)
      val belowIsBlocked = rocks.contains(below) || sand.contains(below)
      val leftIsBlocked = rocks.contains(diagonalLeft) || sand.contains(diagonalLeft)
      val rightIsBlocked = rocks.contains(diagonalRight) || sand.contains(diagonalRight)

      if(belowIsBlocked && leftIsBlocked && rightIsBlocked) {
        sand.addOne(point)
        true
      }else if (!belowIsBlocked) {
        addSand(below)
      }else if(!leftIsBlocked){
        addSand(diagonalLeft)
      }else{
        addSand(diagonalRight)
      }
    }

  }

  @tailrec
  def addUntilBust2(): Unit = {


    if (addSand2((500, 0)) == (500,0)) {
      printGrid()
    } else {
      //println(s"adding more sand: ${updated.size} -- ${sand.size}")
      if (sand.size % 500 == 0) {
        println(s"${sand.size} sand has fallen")
      }
      addUntilBust2()
    }
  }


  @tailrec
  def addSand2(point: (Int, Int)): (Int, Int) = {
      val below = (point._1, point._2 + 1)
      val diagonalLeft = (below._1 - 1, below._2)
      val diagonalRight = (below._1 + 1, below._2)
      val belowIsBlocked = rocks.contains(below) || sand.contains(below) || below._2 == realBottomMost
      val leftIsBlocked = rocks.contains(diagonalLeft) || sand.contains(diagonalLeft) || diagonalLeft._2 == realBottomMost
      val rightIsBlocked = rocks.contains(diagonalRight) || sand.contains(diagonalRight) || diagonalRight._2 == realBottomMost

      if(below._2 == realBottomMost){
        println(point)
      }
      if (belowIsBlocked && leftIsBlocked && rightIsBlocked) {
        sand.addOne(point)
        point
      } else if (!belowIsBlocked) {
        addSand2(below)
      } else if (!leftIsBlocked) {
        addSand2(diagonalLeft)
      } else {
        addSand2(diagonalRight)
      }

  }




  def printGrid() : Unit = {
    (0 to realBottomMost).foreach{ y =>
      val str = (leftMost to rightMost).map{ x =>
        val point = (x -> y)
        if((x -> y) == (500 -> 0)){
          '+'
        }else if(rocks.contains(point) || y == realBottomMost){
          '#'
        }else if(sand.contains(point)){
          'O'
        }else{
          '.'
        }
      }
      println(str.mkString(""))

    }

    println("=========================================")
    println("=========================================")
    println("=========================================")
    println("\n")
    println("\n")
    println("\n")

  }


}
