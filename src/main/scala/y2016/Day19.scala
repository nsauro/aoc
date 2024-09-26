package y2016

import scala.annotation.tailrec
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

  val treeBuilder = mutable.TreeSet.newBuilder[Int]
  treeBuilder.sizeHint(elfCount)
  val tree = treeBuilder.result()
  (1 to elfCount).foldLeft((allElves, list)) {
    case ((m, l), i) => {
      tree.addOne(i)
      (m.addOne(i -> 1), l.addOne(i))
    }
  }

  // println(compute(list))
  println(compute2(tree, 1))

  def compute(l: ListBuffer[Int]): Int = {
    println(s"computing ${l.size} elements")
    if (l.size == 1) {
      l.head
    } else {
      val lb = ListBuffer.newBuilder[Int]
      lb.sizeHint(l.size / 2)
      val newList = lb.result()
      val res = l.sliding(2).foldLeft(newList) { case (acc, i) =>
        if (allElves(i.head) != 0) {
          allElves(i.head) = allElves(i.head) + allElves(i.last)
          allElves(i.last) = 0
          newList.addOne(i.head)
        } else {
          newList
        }
      }
      val realRes = if ((allElves(l.last) != 0)) {
        allElves(l.last) = allElves(l.last) + allElves(l.head)
        res.tail.addOne(l.last)
      } else {
        res
      }
      compute(realRes)
    }
  }

  @tailrec
  def compute2(l: mutable.TreeSet[Int], i: Int): Int = {

    if (l.size % 1000 == 0) {
      println(l.size)
    }

    // println(s"i - $i  elf - ${l(i)} l - $l")
    if (l.size == 1) {
      l.head
    } else {
      val takeFrom: Long = if (i <= l.size / 2) {
        val p = i + Math.floor(l.size.toFloat / 2f)
        if (p >= l.size) 0L else p.toLong
      } else {
        val p = i - Math.ceil(l.size.toFloat / 2f)
        if (p < 0) l.size - 1L else p.toLong
      }
      val takeFromI = takeFrom.toInt
      // println(s"takeFrom: ${l(takeFromI)}")
      //  allElves(l(i)) = allElves(l(i)) + allElves(l(takeFromI))
//      allElves(l(takeFromI)) = 0
      val t0 = System.currentTimeMillis()
      if (l.size <= 989000) {
        //  println("removing")
      }
      if (!l.contains(takeFromI)) {
        println("miss")
      }
      l.remove(takeFromI)
      if (l.size <= 989000) {
        //     println(s"remove took ${System.currentTimeMillis() - t0}")
      }
      val newI = if (i + 1 >= l.size) 0 else i + 1
      compute2(l, newI)
    }

  }
}
