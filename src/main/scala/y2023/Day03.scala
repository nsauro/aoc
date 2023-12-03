package y2023

import scala.io.Source

object Day03 extends App{

  val data = Source.fromResource("2023/3.data").getLines().toSeq.toArray

  val sum = data.zipWithIndex.map{ case (row, i) =>

    val numbers = getNumbers(row.zipWithIndex)
    numbers.map{ case((v, start, end)) =>

      if(isAdjacent(i, start, end)){
        v
      }else{
        0
      }
    }.sum
  }.sum
  println(sum)


  def getNumbers(s : Seq[(Char, Int)], cur : (String, Int, Int) = ("", -1, -1), acc :Seq[(Int, Int, Int)] = Seq.empty) : Seq[(Int, Int, Int)] = {

    if(s.isEmpty){ //nothing left..if we are processing a number, finish it up
      if(cur._1.nonEmpty) {
        acc :+ (cur._1.toInt, cur._2, data.head.length -1)  //haxor
      }else{
        acc
      }
    }else{
      if(s.head._1.isDigit && cur._1.isEmpty){
        //new number..add char to string, note start index, leave end empty
        getNumbers(s.tail, (cur._1 + s.head._1, s.head._2, -1), acc)
      }else if(s.head._1.isDigit && cur._1.nonEmpty){  //accumulating a number
        getNumbers(s.tail, (cur._1 + s.head._1, cur._2, cur._3), acc)
      }else if(cur._1.nonEmpty){ //reached end of a number, since no digit
        getNumbers(s.tail, ("", -1, -1), acc :+ (cur._1.toInt, cur._2, s.head._2 - 1))
      }else{ //no number currently and no digit
        getNumbers(s.tail, cur, acc)
      }
    }
  }

  def isAdjacent(rowNum:Int, start:Int, end:Int) = {

    //get preceding characters
    val preceding = if(start == 0) Seq.empty[Char] else Seq(data(rowNum)(start - 1))
    //get succeeding characters
    val succeeding = if(end == data.head.length - 1) Seq.empty[Char] else Seq(data(rowNum)(end + 1))

    val min = Math.max(0, start - 1)
    val max = Math.min(data.head.length - 1, end + 1)

    //get row above characters
    val above = if(rowNum == 0) Seq.empty[Char] else data(rowNum - 1).substring(min, max +1).toSeq
    //get row below characters
    val below = if(rowNum == data.length - 1) Seq.empty[Char] else(data(rowNum + 1)).substring(min, max +1).toSeq

    (preceding ++ succeeding ++ above ++ below).exists(_ != '.')
  }

  //--------------- part 2  ------------ //


  val numbers = data.map(x => getNumbers(x.zipWithIndex))

  val gearRatio = data.zipWithIndex.map{case  (x, i) =>
    x.zipWithIndex.collect{
      case ('*', gi) => getGearRatio(i, gi)
    }.sum
  }.sum

  println(gearRatio)

  def getGearRatio(row : Int, index: Int) : Long = {

    //preceding number
    val preceding = numbers(row).find(_._3 == index - 1)
    //succeeding number
    val succeeding = numbers(row).find(_._2 == index + 1)
    //numbers above
    val above = if(row == 0) Seq.empty else numbers(row - 1).filter{ n =>
      index >= n._2 -1 && index <= n._3 + 1
    }
    //numbers below
    val below = if (row == data.length - 1) Seq.empty else numbers(row + 1).filter { n =>
      index >= n._2 - 1 && index <= n._3 + 1
    }

    val all = below ++ above ++ preceding ++ succeeding
    if(all.size == 2){
      all.map(_._1.toLong).product
    }else{
      0
    }
  }
}