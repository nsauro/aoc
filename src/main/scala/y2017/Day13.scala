package y2017

import scala.io.Source

object Day13 extends App{

  val data = Source.fromResource("2017/13.data").getLines().toSeq
  val Scanner = """(\d+): (\d+)""".r

  val scanners = data.foldLeft(Map.empty[Int, Int]){
    case (acc, Scanner(n, r)) => acc + (n.toInt -> r.toInt)
  }

  val max = scanners.keys.max

  def doTheThing(i : Int, cost : Int, turn: Int) : Int = {
    if i > max then
      cost
    else
      scanners.get(i) match {
        case None => doTheThing(i + 1, cost, turn + 1)
        case Some(r) => {
          val scannerLoc = computeIndex(r - 1, i)
          val turnCost = if scannerLoc == 0 then
              i * r
            else
              0
            doTheThing(i + 1, cost + turnCost, turn + 1)
        }
      }
  }



  def doTheThing2(i : Int, turn: Int) : Boolean = {
    if i > max then
      true
    else
      scanners.get(i) match {
        case None => doTheThing2(i + 1, turn + 1)
        case Some(r) =>
          val scannerLoc = computeIndex(r - 1, turn)
          if (scannerLoc == 0) then
            false
          else
            doTheThing2(i + 1, turn + 1)
      }
  }

  def part2(offset: Int) : Int = {
    if(doTheThing2(0, offset)) then
      offset
    else
      part2(offset + 1)
  }

  println(doTheThing(0, 0, 0))
  println(part2(0))


  def computeIndex(i : Int, turn: Int) : Int = {
    val goingDown = (turn / i) % 2 == 0
    val offset = turn % i
    if(goingDown) then
      offset
    else
      i - offset
  }
}
