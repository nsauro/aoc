package y2024

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object Day12 extends App:

  val data = Source.fromResource("2024/12.data").getLines().zipWithIndex.flatMap{
    case(s, y) => s.zipWithIndex.map((c, x) => (x,y) -> c)
  }.toMap

  val maxX = data.keys.maxBy(_._1)._1
  val maxY = data.keys.maxBy(_._2)._2

  val plants = mutable.Map.from(data)

  val part1Res = part1(mutable.Set.empty, plants, 0, 0, 'x', 0)
  println(part1Res)

  val part2Res = part2(mutable.Set.empty, mutable.Map.from(data), mutable.Set.empty, 0, 'x', 0)
  println(part2Res)


  @tailrec
  def part1(queue: mutable.Set[(Int, Int)],
    remaining: mutable.Map[(Int, Int), Char],
  curArea: Int,
  curPerimeter: Int,
  curLetter: Char,
  acc: Long) : Long = {
   if queue.isEmpty then //we are done with our current patch
     remaining.headOption match {
        case Some(k,v) =>
          val next = remaining.head
          queue.add(k)
          part1(
            queue,
            remaining.tail,
            0,
            0,
            v,
            acc + (curArea * curPerimeter)
          )
        case None => acc + (curPerimeter * curArea)
      }
  else
    val next = queue.head
    val nextNeighbors = neighbors(next)
    val perimCount = nextNeighbors.count{ p =>
      !isValid(p._1, p._2) || data(p) != curLetter
    }

    val nextInRegion = nextNeighbors.filter{ p =>
      remaining.get(p).contains(curLetter)
    }
    nextInRegion.foreach(remaining.remove)
    part1(
      queue.tail.addAll(nextInRegion),
      remaining,
      curArea + 1,
      perimCount + curPerimeter,
      curLetter,
      acc
    )
  }


  @tailrec
  def part2(queue: mutable.Set[(Int, Int)],
            remaining: mutable.Map[(Int, Int), Char],
            sides: mutable.Set[Perimeter],
            curArea: Int,
            curLetter: Char,
            acc: Long): Long = {
    if queue.isEmpty then //we are done with our current patch
      val totalSides = computeTotalSides(sides)
      println(s"$curLetter has area: $curArea and sides: $totalSides")
      remaining.headOption match {
        case Some(k, v) =>
          val next = remaining.head
          queue.add(k)
          part2(
            queue,
            remaining.tail,
            mutable.Set.empty,
            0,
            v,
            acc + (curArea * totalSides)
          )
        case None => acc + (curArea * totalSides)
      }
    else
      val next = queue.head
      val nextNeighbors = neighbors(next)
      val bounds = nextNeighbors.collect{
        case p if isNotPartOf(p, curLetter) => getBound(next, p)
      }
      if bounds.nonEmpty then
        sides.add(Perimeter(next, bounds.toSet))

      val nextInRegion = nextNeighbors.filter { p =>
        remaining.get(p).contains(curLetter)
      }
      nextInRegion.foreach(remaining.remove)
      part2(
        queue.tail.addAll(nextInRegion),
        remaining,
        sides,
        curArea + 1,
        curLetter,
        acc
      )
  }

  def getBound(good: (Int, Int), bad: (Int, Int)) : Char = {
    if good._1 - bad._1 == 1 then
      'L'
    else if bad._1 - good._1 == 1 then
      'R'
    else if good._2 - bad._2 == 1 then
      'U'
    else
      'D'
  }

  def isNotPartOf(p : (Int, Int), c : Char) : Boolean = {
    !isValid(p._1, p._2) || data(p) != c

  }

  case class Perimeter(p : (Int, Int), bounds: Set[Char])  {
    def countLR = bounds.count(x => x == 'L' || x == 'R')
    def getLR = bounds.filter(x => x == 'L' || x == 'R')
    def countUD = bounds.count(x => x == 'U' || x == 'D')
    def getUD = bounds.filter(x => x == 'U' || x == 'D')
  }

  def computeTotalSides(perimeter: mutable.Set[Perimeter]) : Int = {

    val allXs = perimeter.map(_.p._1).toSet.toSeq
    val allYs = perimeter.map(_.p._2).toSet.toSeq

    //for all Xs, go from top to bottom and count contiguous sides using L
    //then go from bottom to top using R
    val upDownSides = allXs.map{ x =>
      val sides = perimeter.filter(_.p._1 == x).toSeq.sortBy(_.p._2)
      //123 456 7
      val init = sides.head.countLR
      if sides.size == 1 then
        init
      else
        sides.sliding(2).foldLeft(init){ case( acc, pair) =>
          if(pair.head.p._2 - pair.last.p._2 == -1) then //contiguous
            val hLR = pair.head.getLR
            val lLR = pair.last.getLR
            if(lLR.size > hLR.size) then
              acc + (lLR.size - hLR.size)
            else if (lLR.size == 1 && hLR.size == 1 && lLR != hLR) then
              acc + 1
            else
              acc
          else
            acc + pair.last.countLR

        }
    }.sum


    //for all ys, go from top to bottom and count contiguous sides using L
    //then go from bottom to top using R
    val leftRightSides = allYs.map { y =>
      val sides = perimeter.filter(_.p._2 == y).toSeq.sortBy(_.p._1)
      //123 456 7
      val init = sides.head.countUD
      if sides.size == 1 then
        init
      else
        sides.sliding(2).foldLeft(init) { case (acc, pair) =>
          if (pair.head.p._1 - pair.last.p._1 == -1) then //contiguous
            val hud = pair.head.getUD
            val lud = pair.last.getUD
            if(lud.size > hud.size) then
              acc + (lud.size - hud.size)
            else if (lud.size == 1 && hud.size ==1 && lud != hud) then
              acc + 1
            else
              acc
          else
            acc + pair.last.countUD
        }
    }.sum
    upDownSides + leftRightSides

  }


  def getNext(t : (Int, Int), dir: Char) : (Int, Int) = {
    dir match{
      case 'D' => (t._1, t._2 + 1)
      case 'R' => (t._1 + 1, t._2)
      case 'U' => (t._1, t._2 - 1)
      case 'L' => (t._1 - 1, t._2 + 1)
    }
  }

  def isValid(x: Int, y: Int) = y <= maxY && y >= 0 && x <= maxX && x >= 0

  def neighbors(t: (Int, Int)) : Seq[(Int, Int)] = {
    Seq(
      (t._1 + 1, t._2),
      (t._1 - 1, t._2),
      (t._1, t._2 + 1),
      (t._1, t._2 - 1),
    )
  }
