package y2015

import scala.io.Source

object Day23 extends App {

  val data = Source.fromResource("2015/23.data").getLines().toArray

  data.permutations

  val (a, b) = compute(1, 0, 0)

  println(s"a: $a -- b: $b")

  def compute(regA: Int, regB: Int, instPointer: Int): (Int, Int) = {

    if (instPointer >= data.length) {
      (regA, regB)
    } else {
      println(s"executing: ${data(instPointer)}")

      data(instPointer) match {
        case "hlf a"     => compute(regA / 2, regB, instPointer + 1)
        case "hlf b"     => compute(regA, regB / 2, instPointer + 1)
        case "tpl a"     => compute(regA * 3, regB, instPointer + 1)
        case "tpl b"     => compute(regA, regB * 3, instPointer + 1)
        case "inc a"     => compute(regA + 1, regB, instPointer + 1)
        case "inc b"     => compute(regA, regB + 1, instPointer + 1)
        case s"jmp $amt" => compute(regA, regB, instPointer + amt.toInt)
        case s"jie a, $amt" =>
          compute(
            regA,
            regB,
            computeOffset(instPointer, amt.toInt, regA % 2 == 0)
          )
        case s"jie b, $amt" =>
          compute(
            regA,
            regB,
            computeOffset(instPointer, amt.toInt, regB % 2 == 0)
          )
        case s"jio a, $amt" =>
          compute(regA, regB, computeOffset(instPointer, amt.toInt, regA == 1))
        case s"jio b, $amt" =>
          compute(regA, regB, computeOffset(instPointer, amt.toInt, regB == 1))
      }
    }
  }

  def computeOffset(instr: Int, offset: Int, f: Boolean): Int = {
    if (f) {
      instr + offset
    } else {
      instr + 1
    }
  }

}
