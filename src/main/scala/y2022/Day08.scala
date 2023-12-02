package y2022

import scala.io.Source

object Day08 extends App{
  val data = Source.fromResource("2022/Day08").getLines().toArray.map(_.toCharArray.map(_.asDigit))
  val transposed = data.transpose

  val perimeterTrees = ((data.size - 1) * 2) + ((data.head.size - 1) * 2)
  val penultimateRow = data.length - 2
  val penultimateColumn = data.head.length - 2

  //lazy
  val coords = for {
    i <- 1 to penultimateRow
    j <- 1 to penultimateColumn
  }yield {
    (i,j)
  }

  val totalVisible = coords.foldLeft(perimeterTrees){ case (acc, (i, j)) =>
    if(checkVisibility(i, j)){
      acc + 1
    }else{
      acc
    }
  }

  println(totalVisible)


  val bestVisible = coords.map{case (i, j) => computeVisibility(i, j)}.max
  println(bestVisible)


  def checkVisibility(i : Int, j : Int) : Boolean = {
    isVisible(data(i), j) || isVisible(transposed(j), i)
  }


  def isVisible(r : Array[Int], index : Int) = {
    val(left, right) = r.splitAt(index)
    val tree = right.head
    left.max < tree || right.tail.max < tree
  }

  def computeVisibility(i : Int, j: Int) : Int = {
    computeVisibility(data(i), j) * computeVisibility(transposed(j), i)
  }

  def computeVisibility(r : Array[Int], index : Int) : Int = {

    val (left, right) = r.splitAt(index)
    val tree = right.head
    def doIt(i : Array[Int], count : Int) : Int = {
      if(i.isEmpty){
        count
      }else{
        if(i.head < tree){
          doIt(i.tail, count + 1)
        }else {
          count + 1
        }
      }
    }
    val leftSize = doIt(left.reverse, 0)
    val rightSize = doIt(right.tail, 0)
    leftSize * rightSize
  }

}
