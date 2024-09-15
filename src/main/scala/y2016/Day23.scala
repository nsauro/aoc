package y2016

import scala.io.Source
import scala.annotation.tailrec


object Day23 extends App{

  val data = Source.fromResource("2016/23.data").getLines().toArray

  val CopyValue = raw"cpy (.+) ([abcd])".r
  val CopyCell = raw"cpy ([abcd]) ([abcd])".r
  val Inc = raw"inc ([abcd])".r
  val Dec = raw"dec ([abcd])".r
  val JnzCellInt = raw"jnz ([abcd]) (.+)".r
  val JnzCellCell = raw"jnz ([abcd]) ([abcd])".r
  
  val JnzIntInt = raw"jnz (.+) (.+)".r
  val JnzIntCell = raw"jnz (.+) ([abcd])".r

  val Tgl = raw"tgl ([abcd])".r

  val tglSwaps = Map("inc" -> "dec", "cpy" -> "jnz", "dec" -> "inc", "jnz" -> "cpy", "tgl" -> "inc")

  val result = execute(Map("a" -> 12, "b" -> 0, "c" -> 0, "d" -> 0), 0)

  println(result)

  @tailrec
  def execute(registers: Map[String, Int], curInstr : Int) : Map[String, Int] = {
    
    if(curInstr >= data.length){
      registers
    }else{
      //println(s"${data(curInstr)} --- ${registers}")
      data(curInstr) match {
        case CopyCell(fromRegister, toRegister) => execute(registers + (toRegister -> registers(fromRegister)), curInstr + 1)
        case CopyValue(value, register) => execute(registers + (register -> value.toInt), curInstr + 1)
        case Inc(register) => execute(registers + (register -> (registers(register) + 1)), curInstr + 1)
        case Dec(register) => execute(registers + (register -> (registers(register) - 1)), curInstr + 1)
        case JnzCellCell(register, direction) => {
          val next = if(registers(register) != 0) curInstr + registers(direction) else curInstr + 1
          val realNext = if(next == curInstr) curInstr + 1 else next
          execute(registers, realNext)
        }
        case JnzCellInt(register, direction) => {
          val next = if(registers(register) != 0) curInstr + direction.toInt else curInstr + 1
          val realNext = if(next == curInstr) curInstr + 1 else next
          execute(registers, realNext)
        }
        case JnzIntCell(register, direction) => {
          val next = if(register.toInt != 0) curInstr + registers(direction) else curInstr + 1
          val realNext = if(next == curInstr) curInstr + 1 else next
          execute(registers, realNext)
        }
        case JnzIntInt(register, direction) => {
          val next = if(register.toInt != 0) curInstr + direction.toInt else curInstr + 1
          val realNext = if(next == curInstr) curInstr + 1 else next
          execute(registers, realNext)
        }
        
        case Tgl(register) => {
            val nextInd = registers(register) + curInstr
            if(nextInd >= data.size){
                execute(registers, curInstr + 1)
            }else{
                val ins = data(nextInd).substring(0, 3)
                val newI = tglSwaps(ins)
                data(nextInd) = data(nextInd).replace(ins, newI)
                execute(registers, curInstr + 1)
            }
        }
        case x => {
            println(s"$x is invalid")
            execute(registers, curInstr + 1)
        }
      }
    }
  }
  
}
