package y2021

import scala.io.Source

object Day4Part1 extends App {


  val data = Source.fromResource("2021/4.data").getLines()
  val numbers = data.next().split(",").map(_.toInt).toSeq
  data.next() //cheating


  val (boards, lastBoard) = data.foldLeft((Seq.empty[Board], Seq.empty[Seq[Int]])){case ((acc, currBoard), line) =>
    val trimmed = line.trim
    if(trimmed == ""){
      (acc :+ Board(currBoard), Seq.empty)
    }else{
      (acc, currBoard :+ trimmed.split(" +").map(_.toInt))
    }
  }

  val allBoards = boards :+ Board(lastBoard)

  println(allBoards.size)


  def doGame(remainingNumbers : Seq[Int], remainingGames : Seq[Board], lastWinner : Option[Int]) : Int = {



    if(remainingNumbers.isEmpty || remainingGames.isEmpty){
      lastWinner.getOrElse(0)
    }else{

      val (newWinner, losers) = remainingGames.foldLeft((Option.empty[Int], Seq.empty[Board])){case ((last, acc), board) =>
        val result = board.playNumber(remainingNumbers.head)
        if(result.isDefined){
          (result, acc)
        }else{
          (last, acc :+ board)
        }

      }
      doGame(remainingNumbers.tail, losers, newWinner orElse lastWinner)
    }
  }



  println(doGame(numbers, allBoards, None))



}


class Board private(grid : Array[Array[BoardCell]], coordMap : Map[Int, (Int, Int)]) {


  import scala.collection.mutable.ListBuffer

  private val selectedNumbers = ListBuffer.empty[Int]

  def playNumber(number : Int) : Option[Int] = {

    coordMap.get(number).flatMap{ case(row, col) =>
      grid(row)(col).selected = true
      selectedNumbers += number
      if(winningRow(row) || winningColumn(col)){
        Some(unselectedSum * number)
      }else{
        None
      }

    }
  }

  def unselectedSum = {
    val unselectedValues = grid.flatMap{ column =>
      column.collect{
        case cell if(!cell.selected) => cell.value
      }
    }
    unselectedValues.sum
  }

  def winningRow(row : Int)  : Boolean = {
    val won = grid(row).forall(_.selected)
    won
  }

  def winningColumn(column : Int) : Boolean = {
    val won = grid.forall(row => row(column).selected)
    won
  }

  override def toString: String = {
    grid.map(_.mkString(" ")).mkString("\n")

  }
}


object Board {
  def apply(rawRows : Seq[Seq[Int]]) : Board = {

    import scala.collection.mutable.{Map => MMap}


    val gridSize = rawRows.size

    val grid: Array[Array[BoardCell]] = Array.ofDim[BoardCell](gridSize, gridSize)
    val coordMap = MMap.empty[Int, (Int, Int)]


    rawRows.zipWithIndex.foreach{ case (row, rowNumber) =>
      row.zipWithIndex.foreach{ case (number, columnNumber) =>
        grid(rowNumber)(columnNumber) = new BoardCell(number, false)
        coordMap += (number -> (rowNumber,columnNumber))
      }
    }

    new Board(grid, coordMap.toMap)

  }

}

class BoardCell(val value : Int, var selected : Boolean) {

  override def toString: String = {
    val selStr = if(selected) "(S)" else ""
    s"$value$selStr"
  }
}


