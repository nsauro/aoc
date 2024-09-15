package y2016

import scala.Ordering
import scala.io.Source

object Day4 extends App{

  val data = Source.fromResource("2016/4.data").getLines().toSeq
  val Thing = raw"(.+)-(\d+)\[(.+)\]".r

  val rooms = data.collect{
    case Thing(pattern, sectorId, checksum) if toChecksum(pattern).startsWith(checksum) => (decrypt(pattern, sectorId.toInt), sectorId.toInt)
  }

  rooms.foreach(println)


  def toChecksum(s: String): String = {
    val x = s.foldLeft(Map.empty[Char, Int]) {
      case (acc, c) =>
        if(c != '-'){
          val updated = acc.getOrElse(c, 0) + 1
          acc + (c -> updated)
        }else{
          acc
        }
    }

    given Ordering[(Char, Int)] with
      override def compare(x: (Char, Int), y: (Char, Int)): Int =
        if x._2 == y._2 then
          x._1.compare(y._1)
        else
          y._2.compare(x._2)

    val y = x.toSeq.sorted.map(_._1).mkString("")
    y
  }


  def decrypt(s : String, sector: Int)  = {
    s.map{ c =>
      if(c == '-'){
        ' '
      }else{
        rotateChar(c, sector)
      }

    }
  }

  def rotateChar(c : Char, times : Int) : Char = {
    (1.to(times)).foldLeft(c){case (acc, _) =>
      if(acc + 1 == 123){
        'a'
      }else{
        (acc + 1).toChar
      }

    }
  }

}
