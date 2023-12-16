package y2023

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day13 extends App {

  val data = Source.fromResource("2023/13.data").getLines().foldLeft(ListBuffer(ListBuffer.empty[ListBuffer[Char]])) {
    case (acc, s) =>
      if (s.trim == "") {
        acc.addOne(ListBuffer.empty[ListBuffer[Char]])
      } else {
        acc.last.addOne(ListBuffer.empty.addAll(s.toCharArray))
        acc
      }
  }

  val res = data.map { x =>
    val(index, count, isRow) = findReflection(x, -1, -1)
    val res = if(isRow){
      100 * count
    }else{
      count
    }
    res
  }.sum

  println(res)

 val res2 = data.map{ x =>
   val(originalIndex, originalC, originalIsRow) = findReflection(x, -1, -1)

    val points = for {
      r <- x.indices
      c <- x.head.indices
    } yield (
      (r, c)
      )
    val (index, count, isRow) = points.foldLeft((-1, -1, false)) { case (acc, (r, c)) =>
      if (acc == (-1, -1, false)) {
        swap(x, r, c)
        val ignoreRow = if(originalIsRow) originalIndex else -1
        val ignoreCol = if(!originalIsRow) originalIndex else -1
        val a = findReflection(x, ignoreRow, ignoreCol)
        swap(x, r, c)
        a
      } else {
        acc
      }
    }
   val res = if (isRow) {
     100 * count
   } else {
     count
   }
   println(res)
   res
  }.sum

  println(res2)

  def swap(l: ListBuffer[ListBuffer[Char]], r: Int, c: Int): Unit = {
    val existingC = l(r)(c)
    val newC = if (existingC == '#') '.' else {
      '#'
    }
    l(r)(c) = newC
  }


  def findReflection(x: ListBuffer[ListBuffer[Char]], ignoreRow: Int, ignoreCol: Int): (Int, Int, Boolean) = {

    val rowSum = find(x, ignoreRow).map(f => (f._1, f._2, true))
    rowSum.orElse{
      find(x.transpose, ignoreCol) .map(f => (f._1, f._2, false))
    }.getOrElse((-1, -1, false))
  }

  def find(x : ListBuffer[ListBuffer[Char]], ignore: Int) = {
     object Reflection{
       def unapply(rowIndex: Int) : Option[(Int, Int)] = {
         val (top, bottom) = x.splitAt(rowIndex + 1)
         val reversed = top.reverse
         if (reversed.zip(bottom).forall(x => x._1 == x._2)) {
           Some(rowIndex, rowIndex + 1)
         } else {
           None
         }
       }
     }

    x.indices.filter(i => i < x.length - 1 && i != ignore).collectFirst {
      case Reflection(c) => c
    }
  }
}

