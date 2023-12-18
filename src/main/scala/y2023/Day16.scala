package y2023

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object Day16 extends App{

  val raw = Source.fromResource("2023/16.data").getLines().toArray.map(_.toArray)

  val occupiedTiles = findPath(Tile(0,0, Direction.East), mutable.HashSet.empty)

  val mapped = occupiedTiles.map(x => (x.row, x.col))
  println(mapped.size)

  //part 2

  val all = raw.indices.flatMap{ row =>
    Seq(
      Tile(row, 0, Direction.East),
      Tile(row, raw.head.length - 1, Direction.West)
    )
  } ++
    raw.head.indices.flatMap{ col =>
      Seq(
        Tile(0, col, Direction.South),
        Tile(raw.length -1, col, Direction.North)
      )
    }


  val res2F = Future.traverse(all){ x =>
    Future{
      val occupiedTiles = findPath(x, mutable.HashSet.empty)
      val mapped = occupiedTiles.map(x => (x.row, x.col))
      println(mapped.size)
      mapped.size
    }
  }

  val res2 = Await.result(res2F, 5.hours)
  println(res2.max)


  def findPath(current: Tile, acc: mutable.HashSet[Tile]) : mutable.HashSet[Tile] = {
    if(current.row < 0 ||
      current.row == raw.length ||
      current.col < 0 ||
      current.col == raw.head.length ||
      acc.contains(current)){  //at a bounds or we hit a loop
      acc
    }else{
      raw(current.row)(current.col) match {

        case '/' => {
          current.direction match {
            case Direction.North | Direction.South => {
              findPath(current.rotateClockwise, acc + current)
            }
            case _ => findPath(current.rotateCounterClockwise, acc + current)
          }
        }
        case '\\' => {
          current.direction match {
            case Direction.North | Direction.South => {
              findPath(current.rotateCounterClockwise, acc + current)
            }
            case _ => findPath(current.rotateClockwise, acc + current)
          }
        }
        case '|' => current.direction match {
          case Direction.North | Direction.South => findPath(current.moveForward, acc + current)
          case _ =>{
            val first = findPath(current.rotateClockwise, acc + current)
             findPath(current.rotateCounterClockwise, (acc + current) ++ first)
          }
        }
        case '-' => current.direction match {
          case Direction.East | Direction.West => findPath(current.moveForward, acc + current)
          case _ => {
            val first = findPath(current.rotateClockwise, acc + current)
            findPath(current.rotateCounterClockwise, (acc + current) ++ first)
          }
        }
        case _ => {
          findPath(current.moveForward, acc + current)
        }
      }

    }
  }

  case class Tile(row: Int, col:Int , direction: Direction) {
    def moveForward : Tile = {
      direction match{
        case Direction.North => this.copy(row = row - 1)
        case Direction.South => this.copy(row = row + 1)
        case Direction.East => this.copy(col = col + 1)
        case Direction.West => this.copy(col = col - 1)
      }
    }

    def rotateClockwise : Tile = {
      direction match {
        case Direction.North => this.copy(col = col + 1, direction = Direction.East)
        case Direction.South => this.copy(col = col - 1, direction = Direction.West)
        case Direction.East => this.copy(row = row + 1, direction = Direction.South)
        case Direction.West => this.copy(row = row - 1, direction = Direction.North)
      }
    }

    def rotateCounterClockwise: Tile = {
      direction match {
        case Direction.North => this.copy(col = col - 1, direction = Direction.West)
        case Direction.South => this.copy(col = col + 1, direction = Direction.East)
        case Direction.East => this.copy(row = row - 1, direction = Direction.North)
        case Direction.West => this.copy(row = row + 1, direction = Direction.South)
      }
    }
  }

  sealed trait Direction
  object Direction{
    case object East extends Direction
    case object West extends Direction
    case object North extends Direction
    case object South extends Direction
  }



}
