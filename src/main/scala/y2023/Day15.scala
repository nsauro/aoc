package y2023

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day15 extends App{

  val raw = Source.fromResource("2023/15.data").getLines().mkString("")

  val Add = raw"""([a-z]+)=(\d+)""".r
  val Remove = raw"""([a-z]+)-""".r

  val res = raw.split(",").map{ x =>
    x.foldLeft(0){ case (acc, ch) =>
      val a = acc + ch
      val b =  a * 17
      b % 256
    }
  }.sum

  println(res)


  val m = mutable.HashMap.empty[Int, ListBuffer[(String, Int)]]
  (0 to 255).foreach(x => m.addOne((x -> ListBuffer.empty)))



  raw.split(",").collect{
    case Add(l, len) => {
      val list = m(hash(l))
      val ind = list.indexWhere(_._1 == l)
      if(ind >= 0){
        list(ind) = (l, len.toInt)
      }else{
        list.addOne((l, len.toInt))
      }
    }
    case Remove(l) => {
      val list = m(hash(l))
      val ind = list.indexWhere(_._1 == l)
      if(ind >= 0){
        list.remove(ind)
      }
    }
  }

 val res2 = m.map{ case (box, list) =>
   if(list.nonEmpty){
     list.zipWithIndex.map{ case (((_, len), i)) =>
       (box + 1) * (i + 1) * (len)
     }.sum
   }else{
     0
   }
 }.sum

println(res2)
  def hash(x : String) : Int = {
    x.foldLeft(0) { case (acc, ch) =>
      val a = acc + ch
      val b = a * 17
      b % 256
    }
  }
}
