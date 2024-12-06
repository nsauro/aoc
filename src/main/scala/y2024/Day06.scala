package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App:
  val data = Source.fromResource("2024/6.data").getLines().toArray.map(_.toArray)

  val starting = findStarting(0,0)

  println(starting)

  val part1 = countSteps(starting._1, starting._2, Set.empty, "UP")
  println(s"part1: $part1")

  val part2Res = part2(0, 0, 0)
  println(part2Res)


  @tailrec
  def findStarting(x : Int, y: Int) : (Int, Int) = {
    if data(y)(x) == '^' then
      (x,y)
    else
      val n = nextCoord(x,y)
      findStarting(n._1, n._2)
  }

  @tailrec
  def countSteps(x: Int, y: Int, seen : Set[(Int, Int)], direction : String) : Int = {
    val (nextX, nextY) = move(direction, x, y)
    if isValid(nextX, nextY) && data(nextY)(nextX) != '#' then //take step
      countSteps(nextX, nextY, seen + ((nextX, nextY)), direction)
    else if isValid(nextX, nextY) && data(nextY)(nextX) == '#' then //rotate
      countSteps(x, y, seen, turnClockwise(direction))
    else
      seen.size

  }

  @tailrec
  def part2(x: Int, y: Int, count : Int) : Int = {
    if !isValid(x,y) then
      count
    else if data(y)(x) == '.' then
      data(y)(x) = '#'
      val newCount = if (isLoop(starting._1, starting._2, Set.empty, "UP")) count + 1 else count
      data(y)(x) = '.'
      if newCount != count then
        println(s"block put: $x $y")
      val next = nextCoord(x,y)
      part2(next._1, next._2, newCount)
    else
      val next = nextCoord(x,y)
      part2(next._1, next._2, count)

  }

  @tailrec
  def isLoop(x: Int, y: Int, seen: Set[(Int, Int, String)], direction: String): Boolean = {
    val (nextX, nextY) = move(direction, x, y)
    if isValid(nextX, nextY) && data(nextY)(nextX) != '#' then //take step
      isLoop(nextX, nextY, seen, direction)
    else if isValid(nextX, nextY) && data(nextY)(nextX) == '#' then //rotate
      if(seen.contains(nextX, nextY, direction)) then
        true
      else
        isLoop(x, y, seen + ((nextX, nextY, direction)), turnClockwise(direction))
    else
      false

  }

  def move(direction : String, x : Int, y: Int) : (Int, Int) = {
    direction match {
      case "RIGHT" => (x + 1, y)
      case "LEFT" => (x - 1, y)
      case "UP" => (x, y - 1)
      case "DOWN" => (x, y + 1)
    }
  }

  def turnClockwise(direction : String) : String = {
    direction match {
      case "UP" => "RIGHT"
      case "RIGHT" => "DOWN"
      case "DOWN" => "LEFT"
      case "LEFT" => "UP"
      case _ => ""
    }
  }

  def nextCoord(x: Int, y: Int) = {
    if x == data.length - 1 then
      (0, y + 1)
    else
      (x + 1, y)
  }

  def isValid(x: Int, y: Int) = y < data.length && y >= 0 && x < data.head.length && x >= 0