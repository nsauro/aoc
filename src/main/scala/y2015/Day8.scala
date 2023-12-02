package y2015

import scala.io.Source

object Day8 extends App{

  val data = Source.fromResource("2015/8.data").getLines().toSeq

  /*val (totalCodeCount, totalCharCount) = data.foldLeft((0,0)){case (acc, str) =>
    val res = measureString(str, 0, 0)
    (acc._1 + res._1, acc._2 + res._2)
  }*/


  val (totaNewCodeCount, totalExistingCodeCount) = data.foldLeft((0,0)) { case (acc, str) =>
    val res = getEncodedLength(str)
    (acc._1 + res, acc._2 + str.length)
  }

  println(totaNewCodeCount - totalExistingCodeCount)



  def measureString(str : String, codeCount : Int, charCount : Int)  : (Int, Int) = {

    if(str.isEmpty){
      (codeCount, charCount)
    }else{
      val next = str.head
      if(next == '"'){
        measureString(str.tail, codeCount + 1, charCount)
      }
      else if(next == '\\'){
        if(str.tail.head == 'x'){
          val (_, remaining) = str.tail.tail.splitAt(2)
          measureString(remaining, codeCount + 4, charCount + 1)

        }else{
          measureString(str.tail.tail, codeCount + 2, charCount + 1)
        }

      }else{
        measureString(str.tail, codeCount + 1, charCount + 1)
      }
    }
  }

  def getEncodedLength(str : String) : Int = {

    str.replace("""\""", """\\""")
      .replace(""""""", """\"""")
      .length + 2
  }

}
