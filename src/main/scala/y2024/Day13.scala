package y2024

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App:
  val data = Source.fromResource("2024/13.data").getLines().mkString("\n")
  val MachineR = """\d+""".r


  val pieces = data.split("\n\n")
  val machines = pieces.map{ s =>
    val nums = MachineR.findAllIn(s).toArray
    Machine(nums(0).toLong, nums(1).toLong, nums(2).toLong, nums(3).toLong, nums(4).toLong, nums(5).toLong)
  }

  val part1Res = machines.map(diophantne).sum
  println(part1Res)

  val bigger = 10000000000000L
  val part2Res = machines.map(m => diophantne(m.copy(px = m.px + bigger, py = m.py + bigger))).sum
  println(part2Res)

  def computeCost(machine: Machine) : Long = {
    val maxA = Math.min(machine.px / machine.ax, machine.py / machine.ay )
    val maxB = Math.min(machine.px / machine.bx, machine.py / machine.by )
    @tailrec
    def doIt(aPresses: Int, bPresses: Int, curLowest : Long): Long  = {
      if aPresses > maxA then //no point continuing
        Math.max(0, curLowest)
      else
        val sumX = (machine.ax * aPresses) + (machine.bx * bPresses)
        val sumY = (machine.ay * aPresses) + (machine.by * bPresses)
        if sumX == machine.px && sumY == machine.py then //hit it, compute newest low, increase A
          val cost = aPresses * 3L + bPresses
          val newMin = if curLowest == -1 then
            cost
          else
            Math.min(cost, curLowest)
          doIt(aPresses + 1, 0, newMin)
        else if sumX > machine.px || sumY > machine.py then //exceeded, reset A
          doIt(aPresses + 1, 0, curLowest)
        else //still not equal
          doIt(aPresses, bPresses + 1, curLowest)
    }

    val r = doIt(0,0,-1)
    println(s"${machine} == $r")
    r
  }

  //had to look up the math ... not my strong suit
  def diophantne(m: Machine) : Long = {
    /*94 a +22 b = 8400
    34 a +67 b = 5400*/
    //(m.ax *a) + (m.bx *b) = m.px
    //(m.ay *a )+ (m.by *b) = m.py
    val aAlignAx = m.ay * m.ax
    val aAlignBx = m.ay * m.bx
    val aAlignPx = m.ay * m.px

    val aAlignAy = m.ax * m.ay
    val aAlignBy = m.ax * m.by
    val aAlignPy = m.ax * m.py

    val elimAB = aAlignBy - aAlignBx
    val elimAP = aAlignPy - aAlignPx
    val b = elimAP / elimAB

    val a = (m.px - (m.bx * b)) / m.ax
    val checkX = (m.ax * a) + (m.bx * b) == m.px
    val checkY = (m.ay * a) + (m.by * b) == m.py
    if checkX && checkY then
      (a * 3) + b
    else
      0
  }

  case class Machine(ax: Long, ay: Long, bx:Long, by:Long, px:Long, py:Long)
