package y2016

import scala.io.Source

object Day8 extends App{

  val data = Source.fromResource("2016/8.data").getLines().toSeq

  val Rect = raw"""rect (\d+)x(\d+)""".r
  val RotateCol = raw"""rotate column x=(\d+) by (\d+)""".r
  val RotateRow = raw"""rotate row y=(\d+) by (\d+)""".r


  val totalRows = 6
  val totalColumns = 50

  val grid = createGrid(totalRows,totalColumns)

  data.foreach{
    case Rect(cols, rows) => {
      for {
        r <- 0 until rows.toInt
        c <- 0 until cols.toInt
      } {
        grid(r)(c) = true
      }
    }
    case RotateCol(col, amount) => {
      val iCol = col.toInt
      val iAmt = amount.toInt
      val on = (0 until totalRows).collect{
        case r if grid(r)(iCol) => {
          if(r + iAmt >= totalRows) r + iAmt - totalRows else r + iAmt
        }
      }.toSet

      for {
        row <- 0 until totalRows
      }{
        grid(row)(iCol) = on.contains(row)
      }
    }
    case RotateRow(row, amount) => {
      val iRow = row.toInt
      val iAmt = amount.toInt
      val on = (0 until totalColumns).collect {
        case c if grid(iRow)(c) => {
          if (c + iAmt >= totalColumns) c + iAmt - totalColumns else c + iAmt
        }
      }.toSet
      for {
        col <- 0 until totalColumns
      } {
        grid(iRow)(col) = on.contains(col)
      }
    }

  }




  printGrid(grid)

  println(grid.map(_.count(_ == true)).sum)

  def createGrid(rows: Int, columns: Int) : Array[Array[Boolean]] = {
    Array.fill(rows){
      Array.fill(columns)(false)
    }
  }

  def printGrid(g : Array[Array[Boolean]]) : Unit = {
    g.
      foreach{ x =>
      val p = x.map( b => if(b) '#' else '.')
      println(p.mkString(""))
    }
  }

}
