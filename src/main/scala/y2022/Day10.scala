package y2022

import scala.io.Source


object Day10 extends App{

  val data = Source.fromResource("2022/Day10").getLines().toSeq

  val result = compute(data, 1, None, 1, 0, Set(20, 60, 100, 140, 180, 220))
  println(result)


  val screen = compute2(data, None, 1, Seq.empty, Seq.empty)
  screen.foreach(x => println(x.mkString("")))


  def compute(instr :Seq[String], curCycle : Int, curInstr : Option[AddInstr], x : Int, signalStrength : Int, interestedCycles : Set[Int]) : Int = {


    val (updatedStrength, updatedInterestedCycles) = if(interestedCycles.contains(curCycle)){
      (signalStrength + (x * curCycle), interestedCycles - curCycle)
    }else {
      (signalStrength, interestedCycles)
    }

    if(curInstr.isEmpty && instr.isEmpty){
      updatedStrength
    }else {
      curInstr match {
        case Some(Add(v)) => {
          compute(instr, curCycle + 1, None, x + v, updatedStrength, updatedInterestedCycles)
        }
        case Some(DelayAdd(v)) => {
          compute(instr, curCycle + 1, Some(Add(v)), x, updatedStrength, updatedInterestedCycles)
        }
        case None => {
          instr.head match {
            case "noop" => compute(instr.tail, curCycle + 1, None, x, updatedStrength, updatedInterestedCycles)
            case s"addx $v" => compute(instr.tail, curCycle, Some(DelayAdd(v.toInt)), x, updatedStrength, updatedInterestedCycles)
          }
        }
      }
    }
  }


  def compute2(instr :Seq[String], curInstr : Option[Int], x : Int, curRow : Seq[Char], screen : Seq[Seq[Char]]) : Seq[Seq[Char]] = {
    if(instr.isEmpty && curInstr.isEmpty){
      screen :+ curRow
    }else{

      val curCycle = curRow.size

      curInstr match {
        case Some(v) =>
          val updatedRow = curRow :+ (if (x == curCycle || x - 1 == curCycle || x + 1 == curCycle) '#' else '.')
          val (nextRow, nextScreen) = if (updatedRow.size == 40) (Seq.empty, screen :+ updatedRow) else (updatedRow, screen)
          compute2(instr, None, x + v, nextRow, nextScreen)
        case None => {

          val updatedRow = curRow :+ (if (x == curCycle || x - 1 == curCycle || x + 1 == curCycle) '#' else '.')
          val (nextRow, nextScreen) = if (updatedRow.size == 40) (Seq.empty, screen :+ updatedRow) else (updatedRow, screen)
          instr.head match {
            case "noop" => compute2(instr.tail, None, x, nextRow, nextScreen)
            case s"addx $v" => compute2(instr.tail, Some(v.toInt), x, nextRow, nextScreen)
          }
        }
      }
    }
  }



  sealed trait AddInstr

  case class DelayAdd(value : Int) extends AddInstr
  case class Add(value : Int) extends AddInstr

}