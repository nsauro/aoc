package y2015

import scala.io.Source

object Day1 extends  App{

  val data = Source.fromResource("2015/1.data").getLines().next()


  val res = data.foldLeft(0){case (acc, c) =>
    c match {
      case '(' =>acc + 1
      case ')' =>acc - 1
    }
  }

  println(res)

  println(findBasementEntryPosition(data, 0, 1))

  def findBasementEntryPosition(str : String, currentFloor : Int, currentPosition: Int) : Int = {
    val next = str.head
    val updatedFloor = computeNewFloor(currentFloor, next)
    if(updatedFloor == -1){
      currentPosition
    }else{
      findBasementEntryPosition(str.tail, updatedFloor, currentPosition + 1)
    }

  }

  def computeNewFloor(acc : Int, char : Char) : Int = {
    char match {
      case '(' => acc + 1
      case ')' => acc - 1
    }
  }

}
