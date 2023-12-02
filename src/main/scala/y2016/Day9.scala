package y2016

import scala.io.Source

object Day9 extends App{
  val data = Source.fromResource("2016/9.data").getLines().mkString("")

  println(expand2(data))

  //data.foreach(x => println(expand(x)))

  println(expand2("(27x12)(20x12)(13x14)(7x10)(1x12)A"))

  def expand(s : String) : String = {


    def doTheThing(remaining : String, acc : String) : String = {
      if(remaining.isEmpty){
        acc
      }else{
        val openingIndex = remaining.indexOf("(")
        if(openingIndex == -1){ //done
          acc ++ remaining
        }else{
          val (preamble, expansion) = remaining.splitAt(openingIndex) //"abc", "(1x2)stuff"
          val closingIndex = expansion.indexOf(")")
          if(closingIndex== -1) { //not a expansion clause
            acc ++ remaining
          }else{
            val (clause, closing) = expansion.splitAt(closingIndex)
            val (toTakeS, timesS) = clause.tail.splitAt(clause.tail.indexOf("x"))
            val toTake = toTakeS.toInt
            val times = timesS.tail.toInt
            val (toRepeat, last) = closing.tail.splitAt(toTake)
            val block = Seq.fill(times)(toRepeat)
            val updatedAcc = acc ++ preamble ++ block.mkString("")
            doTheThing(last, updatedAcc)
          }
        }
      }
    }
    doTheThing(s, "")
  }


  def expand2(s: String): Long = {


    def doTheThing(remaining: String, acc: Long): Long = {
      if (remaining.isEmpty) {
        acc
      } else {
        val openingIndex = remaining.indexOf("(")
        if (openingIndex == -1) { //done
          acc + remaining.length
        } else {
          val (preamble, expansion) = remaining.splitAt(openingIndex) //"abc", "(1x2)stuff"
          val closingIndex = expansion.indexOf(")")
          if (closingIndex == -1) { //not a expansion clause
            acc + remaining.length
          } else {
            val (clause, closing) = expansion.splitAt(closingIndex)
            val (toTakeS, timesS) = clause.tail.splitAt(clause.tail.indexOf("x"))
            val toTake = toTakeS.toInt
            val times = timesS.tail.toInt
            val (toRepeat, last) = closing.tail.splitAt(toTake)
            val repeated = expand2(toRepeat) * times   //Seq.fill(times)(expand2(toRepeat))
            //val block = Seq.fill(times)(toRepeat)
            val updatedAcc = acc + preamble.length + repeated
            doTheThing(last, updatedAcc)
          }
        }
      }
    }

    doTheThing(s, 0)
  }

}
