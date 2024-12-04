package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day04 extends App:

  val data = Source.fromResource("2024/4.data").getLines().toArray.map(_.toArray)

  val xmas = "XMAS".toArray

  val goRight = (x: Int, y: Int) => (x + 1, y)

  val goLeft = (x: Int, y: Int) => (x - 1, y)

  val goUp = (x: Int, y: Int) => (x, y - 1)

  val goDown = (x: Int, y: Int) => (x, y + 1)

  val goDiagLD = (x: Int, y: Int) => (x - 1, y + 1)

  val goDiagLU = (x: Int, y: Int) => (x - 1, y - 1)

  val goDiagRD = (x: Int, y: Int) => (x + 1, y + 1)

  val goDiagRU = (x: Int, y: Int) => (x + 1, y - 1)

  val moves: Seq[(Int, Int) => (Int, Int)] = Seq(
    goRight,
    goLeft,
    goUp,
    goDown,
    goDiagLD,
    goDiagLU,
    goDiagRD,
    goDiagRU
  )

  val diagMoves = Seq(
    (goDiagLD, goDiagRU),
    (goDiagRD, goDiagLU)
  )

  val part1 = doPart1(0,0,0)
  println(part1)

  val part2 = doPart2(0, 0, 0)
  println(part2)


  @tailrec
  def doPart1(x: Int, y: Int, total : Int) : Int = {
    if y == data.length && x == 0 then
      total
    else if data(y)(x) == 'X' then
      val newTotal = total + moves.count(f => matches(x, y, f, 1))
      val (newX, newY) = nextCoord(x,y)
      doPart1(newX, newY, newTotal)
    else
      val (newX, newY) = nextCoord(x,y)
      doPart1(newX, newY, total)
  }

  def doPart2(x: Int, y: Int, total: Int): Int = {
    if y == data.length && x == 0 then
      total
    else if data(y)(x) == 'A' then
      val xmasMatch = diagMoves.forall(f => isDiagonalMas(x, y, f._1, f._2))
      val newTotal = if xmasMatch then total + 1 else total
      val (newX, newY) = nextCoord(x, y)
      doPart2(newX, newY, newTotal)
    else
      val (newX, newY) = nextCoord(x, y)
      doPart2(newX, newY, total)
  }


  def nextCoord(x: Int, y: Int) = {
    if x == data.length - 1 then
      (0, y + 1)
    else
      (x + 1, y)
  }

  def isValid(x: Int, y: Int) = y < data.length && y >= 0 && x < data.head.length && x >= 0

  @tailrec
  def matches(x : Int, y: Int, move: (Int, Int) => (Int, Int), loc : Int) : Boolean = {
    if loc == xmas.length then //matched all chars
      true
    else
      val (nextX, nextY) = move(x, y)
      if isValid(nextX, nextY) && data(nextY)(nextX) == xmas(loc) then
        matches(nextX, nextY, move, loc + 1)
      else
        false
  }

  def isDiagonalMas(x: Int, y: Int, sMove: (Int, Int) => (Int, Int), mMove: (Int, Int) => (Int, Int)) : Boolean = {
    val s = sMove(x,y)
    val m = mMove(x, y)

    val res = Seq(m, (x, y), s).collect {
      case (xx, yy) if isValid(xx, yy) => data(yy)(xx)
    }.mkString("")

    res == "MAS" || res == "SAM"
  }





