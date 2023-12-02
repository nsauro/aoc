package y2015

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App{


  val data = Source.fromResource("2015/12.data").getLines().mkString("").toVector

  val Number = raw"""[\-\d]+""".r

  //println(Number.findAllIn(data).toSeq.map(_.toInt).sum)

  println(getObjectValue(data.tail, 0, false, data.head == '[')._1)

  //{ is already parsed..head is at the first character after that
  def getObjectValue(v : Vector[Char], acc : Int, isRed : Boolean, inArray : Boolean) : (Int, Vector[Char]) = {
    if(v.isEmpty){
      if (isRed) (0, Vector.empty) else (acc, Vector.empty)
    }else{
      val nextChar = v.head
      nextChar match {
        case ',' => {
          getObjectValue(v.tail, acc, isRed, inArray) // in between properties, keep going
        }
        case '[' =>
          val (value, remaining) = getObjectValue(v.tail, 0, false, true)
          getObjectValue(remaining, acc + value, isRed, inArray)
        case '{' => {
          val (value, remaining) = getObjectValue(v.tail, 0, false, false)
          getObjectValue(remaining, acc + value, isRed, inArray)
        }//object beginning
        case ']' => if (isRed) (0, v.tail) else (acc, v.tail)
        case '}' => if (isRed) (0, v.tail) else (acc, v.tail) //object is done..return 0 or acc
        case '"' => {
          //new property..scan until beginning of value

          val removedPropertyName = if(inArray) v else v.dropWhile(_ != ':').tail

          //determine type
          removedPropertyName.head match {
            case '[' =>{
              val (value, remaining) = getObjectValue(removedPropertyName.tail, 0, false, true)
              getObjectValue(remaining, acc + value, isRed, inArray)
            }

            case '{' => {
              val (value, remaining) = getObjectValue(removedPropertyName.tail, 0, false, false)
              getObjectValue(remaining, acc + value, isRed, inArray)
            }//object beginning
            case '"' => { //string beginning
              val i = removedPropertyName.tail.indexWhere(x => x == '"')
              val (value, remaining) = removedPropertyName.tail.splitAt(i)
              val str = value.mkString("")
              val stillRed = !inArray && (isRed || str == "red")
              getObjectValue(remaining.tail, acc, stillRed, inArray)
            }
            case _ => { //number beginning
              val i = removedPropertyName.indexWhere(x => !(x.isDigit || x == '-'))
              val (value, remaining) = removedPropertyName.splitAt(i)
              getObjectValue(remaining, acc + value.mkString("").toInt, isRed, inArray)
            }
          }
        }
        case _ => { //number beginning
          val i = v.indexWhere(x => !(x.isDigit || x == '-'))
          val (value, remaining) = v.splitAt(i)
          val intValue = value.mkString("").toInt
          getObjectValue(remaining, acc + intValue, isRed, inArray)
        }
      }
    }
  }
}
