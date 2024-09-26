package y2017

import scala.io.Source

object Day08 extends App:
  val data = Source.fromResource("2017/8.data").getLines().toSeq

  val Instr = """(\w+) (\w+) ([-\d]+) if (\w+) ([=<>!]+) ([-\d]+)""".r

  val all = data.foldLeft(Map("MAX" -> 0)) {
    case (acc, Instr(reg, dir, amt, compReg, op, compAmt)) =>
      val regCurr = acc.getOrElse(reg, 0)
      val compRegCurr = acc.getOrElse(compReg, 0)
      val compValue = compAmt.toInt
      val newValue =
        if (dir == "inc")
          regCurr + amt.toInt
        else
          regCurr + (amt.toInt * -1)
      val (updatedMap, updated) = op match
        case "<=" if compRegCurr <= compValue =>
          (acc.updated(reg, newValue), true)
        case (">=") if compRegCurr >= compValue =>
          (acc.updated(reg, newValue), true)
        case ("<") if compRegCurr < compValue =>
          (acc.updated(reg, newValue), true)
        case (">") if compRegCurr > compValue =>
          (acc.updated(reg, newValue), true)
        case ("==") if compRegCurr == compValue =>
          (acc.updated(reg, newValue), true)
        case ("!=") if compRegCurr != compValue =>
          (acc.updated(reg, newValue), true)
        case _ => (acc, false)
      if (updated) then
        updatedMap.updated("MAX", Math.max(newValue, acc("MAX")))
      else updatedMap
    case (acc, _) => {
      acc
    }
  }

  println(all.maxBy(_._2))
