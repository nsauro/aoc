package y2016

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day19 extends App {

  import scala.collection.mutable.LinkedHashMap

  val elfCount = 3012210

  val mapBuilder = LinkedHashMap.newBuilder[Int, Int]
  mapBuilder.sizeHint(elfCount)
  val allElves = mapBuilder.result()

  val listBuilder = ListBuffer.newBuilder[Int]
  listBuilder.sizeHint(elfCount)
  val list = listBuilder.result()


   (1 to elfCount).foldLeft((allElves, list)){case ((m, l), i) => (m.addOne(i -> 1), l.addOne(i))}

  //println(compute(list))
  println(compute2(list, 0))

  def compute(l : ListBuffer[Int]) : Int = {
    println(s"computing ${l.size} elements")
    if(l.size == 1){
      l.head
    }else{
      val  lb = ListBuffer.newBuilder[Int]
      lb.sizeHint(l.size / 2)
      val newList = lb.result()
      val res = l.sliding(2).foldLeft(newList){case (acc, i) =>
        if(allElves(i.head) != 0){
          allElves(i.head) = allElves(i.head) + allElves(i.last)
          allElves(i.last) = 0
          newList.addOne(i.head)
        }else{
          newList
        }
      }
      val realRes = if((allElves(l.last) != 0)){
        allElves(l.last) = allElves(l.last) + allElves(l.head)
        res.tail.addOne(l.last)
      }else{
        res
      }
      compute(realRes )
    }
  }

  def compute2(l: ListBuffer[Int], i : Int) : Int = {

    if(l.size % 1000 == 0){
      println(l.size)
    }

   // println(s"i - $i  elf - ${l(i)} l - $l")
    if(l.size == 1){
      l.head
    }else{
      val takeFrom = if(i <= l.size / 2){
        val p = i + Math.floor(l.size.toFloat / 2F)
        if(p >= l.size) 0 else p
      }else{
        val p = i - Math.ceil(l.size.toFloat / 2F)
        if(p < 0) l.size -1  else p
      }
      val takeFromI = takeFrom.toInt
     // println(s"takeFrom: ${l(takeFromI)}")
    //  allElves(l(i)) = allElves(l(i)) + allElves(l(takeFromI))
//      allElves(l(takeFromI)) = 0
      l.remove(takeFromI)
      val newI = if(i + 1 >= l.size) 0 else i + 1
      compute2(l, newI)
    }

  }
}