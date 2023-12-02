package y2021

import scala.io.Source

object Day12 extends App{

  val data = Source.fromResource("2021/12.data").getLines()

  /*val data = Seq(
    "start-A",
      "start-b",
      "A-c",
      "A-b",
      "b-d",
      "A-end",
      "b-end"
  )

  val data2 = Seq(
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc"
  )

 val data3 = Seq(
    "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW"
  )*/


  val allPaths = data.foldLeft(Set.empty[Path]){case (acc, line) =>
    val cs = line.split("-")
    val cave1 = cs(0)
    val cave2 = cs(1)
    acc + Path(Cave(cave1), Cave(cave2))
  }


  allPaths.foreach(println)
  println("-------")



  //val totalPaths = doTheThing(0, y2021.Start, Set.empty, Seq.empty)
  val totalPaths = doTheThing2(Set.empty, SimpleTraversal(caves = Seq(Start), visitedLittleCaves = Set.empty))
  totalPaths.foreach(x =>println(x.mkString(",")))
  println(totalPaths.size)



  def addVisitedCave(cave : Cave, visitedCaves: Set[LittleCave]) : Set[LittleCave] = {
    cave match{
      case lc : LittleCave => visitedCaves + lc
      case _ => visitedCaves
    }
  }


  def doTheThing(acc: Int, currentCave: Cave, visitedCaves : Set[LittleCave], currentPath : Seq[Cave]) : Int = {

    val updatedVisitedCaves = addVisitedCave(currentCave, visitedCaves)
    val updatedPath = currentPath :+ currentCave
    if(currentCave == End){
      //println(updatedPath)
      acc + 1
    }else{
      val paths = allPaths.filter(_.contains(currentCave))

      val nextCaves = paths.map(_.getDestination(currentCave)).filter{
        case lc : LittleCave => !visitedCaves.contains(lc)
        case Start => false
        case _ => true
      }

      acc + nextCaves.toSeq.map(x => doTheThing(acc, x, updatedVisitedCaves, updatedPath)).sum
    }
  }



  def doTheThing2(acc: Set[Seq[Cave]], traversal : Traversal) : Set[Seq[Cave]] = {

    if(traversal.lastVisitedCave == End){
      //println(traversal.caves.mkString(","))
      acc + traversal.caves
    }else{


     //all adjacent caves, minus start
      val adjacentCaves =  allPaths.collect{
        case path if path.contains(traversal.lastVisitedCave) => path.getDestination(traversal.lastVisitedCave)
      }.filter(_ != Start) //start's never visitable more than once


      traversal match {
        case a : SimpleTraversal => {
          val (little, rest) = adjacentCaves.partition(_.isInstanceOf[LittleCave])

          val complexTraversals = little.map(_.asInstanceOf[LittleCave]).diff(a.visitedLittleCaves).flatMap(x =>
          Seq(
            FilledComplexTraversal(a.caves :+ x, a.visitedLittleCaves), //bb
            SimpleTraversal(a.caves :+ x, a.visitedLittleCaves + x), //b
            ))


          val visitableNonLittle = rest.map(x => a.copy(caves = a.caves :+ x))
          acc ++ ((complexTraversals ++ visitableNonLittle).toSeq.flatMap(x => doTheThing2(acc, x)))

        }
        case a : FilledComplexTraversal => {
          val nextTraversals = adjacentCaves.toSeq.flatMap(x => a.tryVisit(x))
          acc ++ nextTraversals.flatMap(x => doTheThing2(acc, x))

        }
      }
    }
  }
}


sealed trait Traversal {
  def lastVisitedCave : Cave = caves.last
  def caves : Seq[Cave]
  def visitedLittleCaves : Set[LittleCave]
}

case class SimpleTraversal(caves : Seq[Cave], visitedLittleCaves : Set[LittleCave]) extends Traversal


case class FilledComplexTraversal(caves: Seq[Cave], visitedLittleCaves: Set[LittleCave]) extends Traversal{
  def tryVisit(cave : Cave) : Option[Traversal] = {
    cave match {
      case lc : LittleCave if visitedLittleCaves.contains(lc) => None
      case lc  : LittleCave => Some(this.copy(caves = caves :+ lc, visitedLittleCaves = visitedLittleCaves + lc))
      case other => Some(this.copy(caves = caves :+ other))
    }
  }
}




sealed trait Cave{
  def value : String

  override def toString: String = value
}

object Cave {
  def apply(str : String) : Cave = {
    if(str == "start"){
      Start
    }else if(str == "end") {
      End
    }else if(str.toLowerCase == str){
      LittleCave(str)
    }else{
      BigCave(str)
    }
  }
}


case object Start extends Cave{
  val value = "start"
}

case object End extends Cave{
  val value = "end"
}
case class BigCave(value : String) extends Cave
case class LittleCave(value : String) extends Cave

case class Path(first : Cave, second : Cave) {

  def contains(cave : Cave) : Boolean = {
    first == cave || second == cave
  }

  def getDestination(from : Cave) : Cave = {
    if(first == from){
      second
    }else if(second == from){
      first
    }else{
      throw new Exception("AOC!")
    }
  }

  override def equals(obj: Any): Boolean = {
    if(obj == null){
      false
    }else{
      obj match {
        case Path(f, s) => Set(f,s) == Set(first, second)
        case _ => false
      }
    }

  }

  override def toString: String = s"$first-$second"
}
