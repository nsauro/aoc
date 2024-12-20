package y2024

object Day17 extends App:


  val program = "2,4,1,5,7,5,0,3,4,0,1,6,5,5,3,0".split(",").map(_.toInt)
  val desired = program.mkString(",")



  println(part1(program, 0, 46187030, 0, 0, Seq.empty))

  println(part2(Math.pow(2, 45).toLong - 1, 0))

  def part1(ins: Array[Int], pointer: Int, a : Long, b: Long, c: Long, acc : Seq[String]) : String = {
    if pointer >= ins.length then
      acc.mkString(",")
    else
      ins(pointer) match {
        case 0 =>
          val denom = Math.pow(2, getComboValue(ins(pointer +1), a, b, c))
          val res = (a / denom).toLong
          part1(ins, pointer + 2, res, b, c, acc)
        case 1 =>
          val res = b ^ ins(pointer + 1)
          part1(ins, pointer + 2, a, res, c, acc)
        case 2 =>
          val res = getComboValue(ins(pointer + 1), a, b, c) % 8
          part1(ins, pointer + 2, a, res, c, acc)
        case 3 if a == 0 => part1(ins, pointer + 2, a, b, c, acc)
        case 3 => part1(ins, ins(pointer + 1), a, b, c, acc)
        case 4 =>
          val res = b ^ c
          part1(ins, pointer + 2, a, res, c, acc)
        case 5 =>
          val res = getComboValue(ins(pointer + 1), a, b, c) % 8
          part1(ins, pointer + 2, a,b,c, acc :+ res.toString)
        case 6 =>
          val denom = Math.pow(2, getComboValue(ins(pointer +1), a, b, c))
          val res = (a / denom).toLong
          part1(ins, pointer + 2, a, res, c, acc)
        case 7 =>
          val denom = Math.pow(2, getComboValue(ins(pointer +1), a, b, c))
          val res = (a / denom).toLong
          part1(ins, pointer + 2, a, b, res, acc)
      }
  }

  def part2(i : Long, prevLength: Int) : Long = {
    val res = part1(program, 0, i, 0, 0, Seq.empty)
    if res == desired then
      i
    else

      println(s"$i: $res")
      part2(i + 1, res.length)
  }

  def getComboValue(i : Int, a: Long, b: Long, c: Long) : Long = {
    i match {
      case 4 => a
      case 5 => b
      case 6 => c
      case 7 => throw new Exception("Blargh")
      case _ => i
    }
  }