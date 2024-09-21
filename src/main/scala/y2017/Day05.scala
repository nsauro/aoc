package y2017

import scala.io.Source

object Day05 extends App{

  val data: Array[Int] =
    Source.fromResource("2017/5.data").getLines().toArray.map(_.toInt)

  println(doTheThing(0,0))

  def doTheThing(i : Int, steps: Int) : Int = {
    if(i >= data.length) then
      steps
    else
      val newI = i + data(i)
      if(data(i) >= 3) then 
        data(i) = data(i) - 1
      else  
        data(i) = data(i) + 1
      doTheThing(newI, steps + 1)
  }

}
