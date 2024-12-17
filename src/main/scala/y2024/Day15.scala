package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day15 extends App:

  type Move = ((Int, Int)) => (Int, Int)
  val grid = Source.fromResource("2024/15.grid.data").getLines().map(_.toCharArray).toArray

  val part2Grid = grid.map(x => x.flatMap{
    case '#' => Seq('#','#')
    case 'O' => Seq('[', ']')
    case '.' => Seq('.', '.')
    case _ => Seq('@', '.')
  })

  val moves = Source.fromResource("2024/15.moves.data").getLines().mkString("")

  val part1Res = processMoves(findBot(grid, 0,0), moves)
  println(part1Res)

  val part2Res = processMoves2(findBot(part2Grid, 0, 0), moves)
  println(part2Res)

  def processMoves(botLocation: (Int, Int), s : Seq[Char]) : Long = {

    if(s.isEmpty) then
      computeGPS(grid, 0,0,0, 'O')
    else
      val newLoc = s.head match {
        case '>' => move(botLocation, moveRight, moveLeft)
        case '<' => move(botLocation, moveLeft, moveRight)
        case '^' => move(botLocation, moveUp, moveDown)
        case 'v' => move(botLocation, moveDown, moveUp)
      }
      grid(botLocation._2)(botLocation._1) = '.'
      grid(newLoc._2)(newLoc._1) = '@'
      processMoves(newLoc, s.tail)

  }

  def move(loc: (Int,Int), forward: Move, backward: Move) : (Int, Int) = {
    val n : (Int, Int) = forward(loc)
    grid(n._2)(n._1) match {
      case '#' => loc
      case 'O' =>
        val newLoc = move(n, forward, backward)
        grid(newLoc._2)(newLoc._1) = 'O'
        grid(loc._2)(loc._1) = '.'
        backward(newLoc)
      case '.' => n
    }
  }

  ////////////////////part 2

  def processMoves2(botLocation: (Int, Int), s: Seq[Char]): Long = {

    if (s.isEmpty) then
      computeGPS(part2Grid, 0, 0, 0, '[')
    else
      val (newLoc, boxesMoved) = s.head match {
        case '>' => moveHorizontal(botLocation, moveRight, moveLeft, false)
        case '<' => moveHorizontal(botLocation, moveLeft, moveRight, false)
        case '^' =>  moveVertical(botLocation, moveUp)
        case 'v' => moveVertical(botLocation, moveDown)
      }
      part2Grid(botLocation._2)(botLocation._1) = '.'
      part2Grid(newLoc._2)(newLoc._1) = '@'
      processMoves2(newLoc, s.tail)

  }

  def moveHorizontal(loc: (Int, Int), forward: Move, backward: Move, boxesMoved: Boolean) : ((Int, Int), Boolean) = {
    val n: (Int, Int) = forward(loc)
    part2Grid(n._2)(n._1) match {
      case '#' => (loc, boxesMoved)
      case ']' | '[' =>
        val (newLoc, _) = moveHorizontal(n, forward, backward, true)
        part2Grid(newLoc._2)(newLoc._1) = part2Grid(n._2)(n._1)
        (backward(newLoc), true)
      case '.' => (n, boxesMoved)
    }
  }

  def moveVertical(loc: (Int, Int), forward: Move): ((Int, Int), Boolean) = {
    val n: (Int, Int) = forward(loc)
    val (moved, boxesMoved) = part2Grid(n._2)(n._1) match {
      case '#' => (false, false)
      case '.' => (true, false)
      case '[' =>
        val r = tryMoveVertical(Seq(n), forward, false)
        if r then
          tryMoveVertical(Seq(n), forward, true)
        (r, r)
      case ']' =>
        val r = tryMoveVertical(Seq(n.copy(_1 = n._1 - 1)), forward, false)
        if r then
          tryMoveVertical(Seq(n.copy(_1 = n._1 - 1)), forward, true)
        (r,r)
    }
    if moved then
      (n, boxesMoved)
    else
      (loc, boxesMoved)
  }

  //each coord is the left side
  def tryMoveVertical(s : Seq[(Int, Int)], m: Move, doMove: Boolean) : Boolean = {

    val res = s.foldLeft(true){ case (acc, lhs) =>
      if acc then //only keep going if we are still good
        val next = m(lhs)
        val rhs = next.copy(_1 = next._1 + 1)
        acc && ((part2Grid(next._2)(next._1), part2Grid(rhs._2)(rhs._1)) match {
          case ('.', '.') => true
          case ('#', _) | ( _, '#') => false
          case ('[', ']') => tryMoveVertical(Seq(next), m, doMove)
          case (']', '[') => tryMoveVertical(Seq(next.copy(_1 = next._1 - 1), rhs), m, doMove)
          case ('.', '[') => tryMoveVertical(Seq(rhs), m, doMove)
          case (']', '.') => tryMoveVertical(Seq(next.copy(_1 = next._1 - 1)), m, doMove)
          case _ => println(s"OMFG:"); false
        })
      else
        acc
    }

    if res && doMove then
      s.foreach{ lhs =>
        val next = m(lhs)
        part2Grid(next._2)(next._1) = '['
        part2Grid(next._2)(next._1 + 1) = ']'
        part2Grid(lhs._2)(lhs._1) = '.'
        part2Grid(lhs._2)(lhs._1 + 1) = '.'
      }
    res
  }

  @tailrec
  def findBot(g: Array[Array[Char]], x: Int, y: Int) : (Int, Int) = {
    if g(y)(x) == '@' then
      (x,y)
    else
      if y + 1 == grid.head.length then
        findBot(g,x + 1, 0)
      else
        findBot(g,x, y + 1)
  }


  @tailrec
  def computeGPS(g: Array[Array[Char]], x : Int, y: Int, acc: Long, char: Char) : Long = {
    if y >= g.length then
      acc
    else
      val newAcc =
        if g(y)(x) == char then
          acc + (100 * y + x)
        else
          acc
      if x + 1 == g.head.length then
        computeGPS(g, 0, y + 1, newAcc, char)
      else
        computeGPS(g, x + 1, y, newAcc, char)
  }

  def moveRight(p :(Int, Int)) = (p._1 + 1, p._2)
  def moveLeft(p :(Int, Int)) = (p._1 - 1, p._2)
  def moveUp(p : (Int, Int))  = (p._1, p._2 - 1)
  def moveDown(p :(Int, Int)) = (p._1, p._2 + 1)
