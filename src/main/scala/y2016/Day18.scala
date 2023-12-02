package y2016

object Day18 extends App {

  val startRow = ".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^.".toCharArray
  val rowSize = startRow.size

  val traps =Seq(
    Seq('^', '^', '.'),
    Seq('.', '^', '^'),
    Seq('^', '.', '.'),
    Seq('.', '.', '^'),
  )


  println(compute(399999, startRow, startRow.count(_ == '.')))


  def compute(timesRemaining: Int, previousRow: Seq[Char], totalTraps: Int) : Int = {

    //println(previousRow.mkString(" "))
    if(timesRemaining == 0){
      totalTraps
    }else{

      val nextRow = previousRow.zipWithIndex.map{
        case(c, i) => {
          val segment = Seq(i - 1, i, i + 1).map{
            case -1 | `rowSize` => '.'
            case other => previousRow(other)
          }
          if(traps.contains(segment)){
            '^'
          }else{
            '.'
          }



        }
      }
      compute(timesRemaining - 1, nextRow, totalTraps + nextRow.count(_ == '.'))
    }


  }

}
