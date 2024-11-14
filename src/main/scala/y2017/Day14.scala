package y2017

import scala.collection.mutable

object Day14  extends App{


  val used = (0 to 127).map{ i =>
    knotHash(s"stpzcrnm-$i").count(_ == '1')
  }.sum

  println(used)

  val allHashes = (0 to 127).map { i =>
    knotHash(s"stpzcrnm-$i").toArray
  }.toArray
  

  val allIndices =
    for{
      i <- 0 to 127
      j <- 0 to 127
    } yield{
      (i,j)
    }

  val allIndicesSet = mutable.LinkedHashSet(allIndices*)
  println(getRegions(allIndicesSet, 0))


  def getRegions(indices : mutable.Set[(Int, Int)], count: Int) : Int =
    if(indices.isEmpty) then
      count
    else
      if allHashes(indices.head._1)(indices.head._2) == '1' then
        val region = mapRegion(Set(indices.head), Set.empty)
        val updated = indices.subtractAll(region)
        getRegions(updated, count + 1)
      else
        getRegions(indices.tail, count)



  def mapRegion(queue: Set[(Int, Int)], visited: Set[(Int, Int)]) : Set[(Int, Int)] = {
    if(queue.isEmpty) then
      visited
    else
      val neighbors = getNeighbors(queue.head._1, queue.head._2).toSet
      val newNeighbors = neighbors.diff(visited)
      mapRegion(queue.tail ++ newNeighbors, visited + queue.head)
  }

  def getNeighbors(i: Int, j: Int) = {
    Seq(
      (i - 1, j),
      (i + 1, j),
      (i, j - 1),
      (i, j + 1)
    ).filter{ case(ii, jj) =>
      ii >= 0 && ii < allHashes.length && jj >= 0 && jj < allHashes.head.length && allHashes(ii)(jj) == '1'
    }

  }
  
  def knotHash(input:String): String = {

    val h = (0L to 255L).toArray
    val asciiCodes = input.toSeq.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)

    def doIt(timesLeft: Int, pos: Int, skipSize: Int): Unit = {
      if timesLeft != 0 then
        val (updatedH, newPos, newSkipSize) = doTheHash(h, asciiCodes, pos, skipSize)
        doIt(timesLeft - 1, newPos, newSkipSize)
    }

    doIt(64, 0, 0)
    h.grouped(16).map { x =>
      val str = x.reduce(_ ^ _).toBinaryString
      (Array.fill(8 - str.length)('0')).mkString("") + str
    }.mkString("")
  }

  def doTheHash(h: Array[Long], l: Seq[Int], pos: Int, skipSize: Int): (Array[Long], Int, Int) = {
    if l.isEmpty then
      (h, pos, skipSize)
    else
      val indices = (pos until pos + l.head)
      val elements = indices.map { i =>
        if i < h.length then
          h(i)
        else
          h(i % h.length)
      }.reverse
      (indices zip elements).foreach {
        case (i, e) =>
          if i < h.length then
            h(i) = e
          else
            h(i % h.length) = e
      }
      doTheHash(h, l.tail, pos + l.head + skipSize, skipSize + 1)
  }

}
