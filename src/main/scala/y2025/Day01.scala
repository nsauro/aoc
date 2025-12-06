package y2025

import scala.io.Source

object Day01 extends App{

  val data = Source.fromResource("2025/1.data").getLines().toSeq


  val part1 = data.foldLeft((50,0)){ case((pos, count), step) =>
    val dir = if(step.head == 'L') -1 else 1
    val newPosTmp = (pos + (step.tail.toInt * dir)) % 100
    val newPos = if(newPosTmp < 0) 100 + newPosTmp else newPosTmp
    val newCount = if(newPos == 0) count + 1 else count
    (newPos, newCount)
  }
  println(part1._2)


  val part2 = data.foldLeft((50, 0)) { case ((pos, count), step) =>
    val dir = if (step.head == 'L') -1 else 1
    val steps = step.tail.toInt
    val passes = steps / 100
    val remainder = steps % 100

    if (remainder == 0) {
      (pos, count + passes)
    } else {
      val newPosTmp = (pos + (remainder * dir)) % 100
      val newPos = if (newPosTmp < 0) 100 + newPosTmp else newPosTmp
      if(newPos == 0){
        (newPos, count + passes + 1)
      } else if(dir == -1 && newPos > pos && pos != 0){
        (newPos, count + passes + 1)
      } else if(dir == 1 && newPos < pos && pos != 0) {
        (newPos, count + passes + 1)
      } else{
        (newPos, count + passes)
      }
    }



  }
  println(part2._2)


}
