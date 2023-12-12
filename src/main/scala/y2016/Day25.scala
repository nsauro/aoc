package y2016

import scala.io.Source

object Day25 extends App{

  val data = Source.fromResource("2016/25.data").getLines().toArray

  val CopyValue = raw"cpy (\d+) ([abcd])".r
  val CopyCell = raw"cpy ([abcd]) ([abcd])".r
  val Inc = raw"inc ([abcd])".r
  val Dec = raw"dec ([abcd])".r
  val JCellnz = raw"jnz ([abcd]) (.+)".r
  val JValuenz = raw"jnz (.+) (.+)".r
  val OutValue = raw"out (\d+)".r
  val OutCell = raw"out ([abcd])".r


  def execute(registers: Map[String, Int], curInstr: Int): Map[String, Int] = {
    if (curInstr >= data.length) {
      registers
    } else {
      data(curInstr) match {
        case CopyValue(value, register) => execute(registers + (register -> value.toInt), curInstr + 1)
        case CopyCell(fromRegister, toRegister) => execute(registers + (toRegister -> registers(fromRegister)), curInstr + 1)
        case Inc(register) => execute(registers + (register -> (registers(register) + 1)), curInstr + 1)
        case Dec(register) => execute(registers + (register -> (registers(register) - 1)), curInstr + 1)
        case JCellnz(register, direction) => {
          val next = if (registers(register) != 0) curInstr + direction.toInt else curInstr + 1
          execute(registers, next)
        }
        case JValuenz(value, direction) => {
          val next = if (value.toInt != 0) curInstr + direction.toInt else curInstr + 1
          execute(registers, next)
        }
      }
    }
  }
}
