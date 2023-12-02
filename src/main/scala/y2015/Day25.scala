package y2015

object Day25 extends App{

  val count = figureOutCount(3010, 3019)
  println(count)
  val code = compute(count - 1, 20151125L)
  println(code)
  //value at row $row, column 1
  def figureOutCount(row : Int, column : Int) : Int  = {
    val startingRowValue = (1 to row).foldLeft(1){case (acc, i) => acc + (i - 1)}
    //our final value
    (2 to column).foldLeft(startingRowValue){case (acc, i) =>
      acc + (row + i - 1)
    }

  }

  def compute(count : Int, value : Long) : Long = {
    if(count == 0){
      value
    }else{
      compute(count - 1,  (value * 252533) % 33554393)
    }
  }

}
