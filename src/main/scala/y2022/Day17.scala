package y2022

import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.io.Source

object Day17 extends App{

  val jets = Source.fromResource("2022/Day17").getLines().toSeq.head.toCharArray
  var jIndex = 0
  println(jets.size)
  val streams = Iterator.continually(jets).flatten

  val chamber = ListBuffer.fill(1)(ListBuffer.fill(7)('#')).addAll(ListBuffer.fill(4)(ListBuffer.fill(7)('.')))
  //val occupiedPoints = scala.collection.mutable.Set.empty[(Long, Int)]

  var cycleStarts  = MMap.empty[String, ListBuffer[(Int, Int)]]



  case class Shape(name: String, fillChar : Char, points : Set[(Int, Int)]) {
    def moveLeft = {
      val left = points.map(x => (x._1, x._2 - 1))
      //println(s"move left: $left")

      if(left.exists(p => {
        p._2 < 0 || chamber(p._1)(p._2) != '.'
      })) this else{
        Shape(name, fillChar, left)
      }
    }

    def moveRight = {
      val right = points.map(x => (x._1, x._2 + 1))
      //println(s"move right: $right")
      if (right.exists(p => {
        p._2 > 6 || chamber(p._1)(p._2) != '.'
      })) this else {
        Shape(name, fillChar, right)
      }
    }

    def moveDown = {
      val down = points.map(x => (x._1 - 1, x._2))
      //println(s"move down: $down")
      if (down.exists(p => {
        p._1 <= 0 || chamber(p._1)(p._2) != '.'
      })) this else {
        Shape(name, fillChar, down)
      }
    }
  }

  val max = dropPieces(1, 2022, 0)
  println(max)
  printChamber()
  //printChamber(max)




  /*def drop2(currentPiece : Long, maxPiece : Long) : Long = {
    //drop until cycle detected
    //
    val localChamber = ListBuffer.fill(1)(ListBuffer.fill(7)('#')).addAll(ListBuffer.fill(4)(ListBuffer.fill(7)('.')))
    dropUntilCycleDetected + computeCycleAmount + computeRemaining
  }*/




  def dropUntilCycleDetected(curPiece: Int, localChamber : ListBuffer[ListBuffer[Char]], highestBlock: Int) = {
    val nextPiece = getNextPiece(curPiece, highestBlock)

  }


  def movePiece2(piece: Shape, currentPiece: Int, currentHighest: Int): Int = {
    if (jIndex == jets.size) {
      jIndex = 0
    }
    if (jIndex == 0) {
      checkForCycle(piece, currentHighest, currentPiece)
    }
    val moved = streams.next() match {
      case '>' => piece.moveRight
      case '<' => piece.moveLeft
    }
    jIndex += 1

    val down = moved.moveDown
    if (down == moved) { //couldn't move
      //add to grid
      val highestBlock = down.points.map(_._1).max
      //occupiedPoints.addAll(down.points)
      down.points.foreach { case (row, col) =>
        chamber(row)(col) = piece.fillChar
      }
      val rowsToAdd = 10 - (chamber.size - highestBlock)
      chamber.addAll(ListBuffer.fill(rowsToAdd)(ListBuffer.fill(7)('.'))) //add more rows
      //printChamber()
      highestBlock
    } else {
      movePiece(down, currentPiece, currentHighest)
    }
  }









  def dropPieces(currentPiece: Int, maxPiece :Int, highestBlock : Int) : Long = {
    if(currentPiece > maxPiece){
      highestBlock
    }else{
      if(currentPiece >= 10000 && currentPiece % 10000 == 0){
        println(s"processing piece: $currentPiece")
      }
      val nextPiece = getNextPiece(currentPiece, highestBlock)
      //println(nextPiece)
      val blockHighest = movePiece(nextPiece, currentPiece, highestBlock)
      dropPieces(currentPiece + 1, maxPiece, Math.max(blockHighest, highestBlock))

    }

  }

  def movePiece(piece: Shape, currentPiece:Int, currentHighest : Int) : Int = {
    if(jIndex == jets.size){
      jIndex = 0
    }
    if(jIndex == 0) {
      checkForCycle(piece, currentHighest, currentPiece)
    }
    val moved = streams.next() match {
      case '>' => piece.moveRight
      case '<' => piece.moveLeft
    }
    jIndex += 1

    val down = moved.moveDown
    if(down == moved){ //couldn't move
      //add to grid
      val highestBlock = down.points.map(_._1).max
      //occupiedPoints.addAll(down.points)
      down.points.foreach{ case(row, col) =>
        chamber(row)(col) = piece.fillChar
      }
      val rowsToAdd = 10 - (chamber.size - highestBlock)
      chamber.addAll(ListBuffer.fill(rowsToAdd)(ListBuffer.fill(7)('.'))) //add more rows
      //printChamber()
      highestBlock
    }else{
      movePiece(down, currentPiece, currentHighest)
    }
  }

  def checkForCycle(piece : Shape, curHighest: Int, curPieceCount: Int)  = {
    val previousStarts = cycleStarts.getOrElse(piece.name, ListBuffer.empty)
    previousStarts.addOne((curHighest, curPieceCount))
    cycleStarts(piece.name) = previousStarts
    if(previousStarts.size < 3){
      false
    }else{
      val first = chamber.slice(previousStarts(0)._1, previousStarts(1)._1)
      val second = chamber.slice(previousStarts(1)._1, previousStarts(2)._1)
        if(first == second){
          val piecesSeen = previousStarts(1)._2  - previousStarts(0)._2
          val heightGained = previousStarts(1)._1  - previousStarts(0)._1
          println(s"cycle found!! ${piece.name} pieces: $piecesSeen -- height gained: $heightGained  $previousStarts")
        }
      false
    }
  }

  def getNextPiece(piece : Int, highestBlock: Int) : Shape = {
    val (name, fillChar,  points) = piece % 5 match {
      case 1 => ("DASH", 'D', (2 to 5).map(col  => (highestBlock + 4, col))) //-=-- piece
      case 2 => ("PLUS", 'P', Set((highestBlock + 5, 2), (highestBlock + 5, 3), (highestBlock  + 5, 4), (highestBlock + 4, 3), (highestBlock + 6, 3))) // + piece
      case 3 => ("L", 'L', Set((highestBlock + 4, 2), (highestBlock + 4, 3), (highestBlock + 4, 4), (highestBlock + 5, 4), (highestBlock + 6, 4))) // L piece
      case 4 => ("BAR", 'B',(highestBlock+4 to highestBlock + 7).map(x => (x, 2))) // | piece
      case 0 => ("BLOCK", 'K', Set((highestBlock + 4, 2), (highestBlock + 4, 3), (highestBlock + 5, 2), (highestBlock + 5, 3)))
    }

    Shape(name, fillChar, points.toSet)
  }

  def printChamber() : Unit = {

    println("----------------------------")
    chamber.zipWithIndex.reverse.foreach{ case(r, i) =>
      println(s"|${r.mkString("")}| -- $i")
    }
  }

/*  def printChamber(max : Long) : Unit  = {
    for {
      row <- (1L to max).reverse
    } {
      val r = (0 to 6).map(c => if(occupiedPoints.contains(row -> c)) '#' else '.')
      println(s"|${r.mkString("")}|")
    }
  }*/

}
