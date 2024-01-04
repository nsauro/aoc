package y2023

import java.util.PriorityQueue
import scala.collection.mutable
import scala.io.Source

object Day17 extends App {

  val data = Source.fromResource("2023/17.data").getLines().toArray.map(_.toArray.map(x => x.asDigit))


  val allVisited = dijktras(0,0)
 /* val n = allVisited.filter(n => n.r == data.length - 1 && n.c == data.head.length-1).minBy(_.heatLoss)

  println(n)*/




  def dijktras(r: Int, c: Int) : mutable.HashSet[String] = {


    val visited = mutable.HashSet.empty[String]
    val inFlight = mutable.HashMap.empty[String, Node]
    val q = new PriorityQueue[Node]()
    q.add(Node(r, c, data(r)(c), Direction.East, 0, 10, null))
    q.add(Node(r, c, data(r)(c), Direction.South, 0, 10, null))

    def update(n : Node) : Unit = {
      if (!visited.contains(n.id)) {
        inFlight.get(n.id) match {
          case Some(x) if x.heatLoss > n.heatLoss => {
            q.remove(x)
            q.add(n)
            inFlight.put(n.id, n)
          }
          case None => {
            q.add(n)
            inFlight.put(n.id, n)
          }
          case _ => {
            //existing is already smaller
          }
        }
      }
    }

    while(q.size() != 0){
      val node = q.poll()
      visited.add(node.id)
      inFlight.remove(node.id)
      if(node.r != data.length -1 || node.c != data.head.length - 1){

        if(node.canMoveEast){
          val c = data(node.r)(node.c + 1)
          val n = Node(node.r, node.c + 1, c, Direction.East, node.heatLoss + c, if(node.direction == Direction.East) node.forwardMovesLeft - 1 else 9, node)
          update(n)
        }

        if(node.canMoveWest){
          val c = data(node.r)(node.c - 1)
          val n = Node(node.r, node.c - 1, c, Direction.West, node.heatLoss + c, if(node.direction == Direction.West) node.forwardMovesLeft - 1 else 9, node)
          update(n)
        }

        if(node.canMoveNorth){
          val c = data(node.r - 1)(node.c)
          val n = Node(node.r - 1, node.c, c, Direction.North, node.heatLoss + c, if(node.direction == Direction.North) node.forwardMovesLeft - 1 else 9, node)
          update(n)
        }

        if(node.canMoveSouth){
          val c = data(node.r + 1)(node.c)
          val n = Node(node.r + 1, node.c, c, Direction.South, node.heatLoss + c, if(node.direction == Direction.South) node.forwardMovesLeft - 1 else 9, node)
          update(n)
        }
      }else{
        println(node)
      }
    }
    visited
  }



  case class Node(r : Int, c: Int, cost: Int, direction: Direction, heatLoss: Int, forwardMovesLeft: Int, parent: Node)  extends Comparable[Node]{

    val id = s"$r-$c-$cost-$direction-$forwardMovesLeft"
    def canMoveNorth = {
      val blocked = direction == Direction.North && forwardMovesLeft == 0
      val turnBlocked = direction != Direction.North && forwardMovesLeft > 6
      r > 0 && !blocked && direction != Direction.South && !turnBlocked
    }

    def canMoveSouth = {
      val blocked = direction == Direction.South && forwardMovesLeft == 0
      val turnBlocked = direction != Direction.South && forwardMovesLeft > 6
      r < data.length - 1 && !blocked && direction != Direction.North && !turnBlocked
    }

    def canMoveEast = {

      val blocked = direction == Direction.East && forwardMovesLeft == 0
      val turnBlocked = direction != Direction.East && forwardMovesLeft > 6
      c < data.head.length - 1 && !blocked && direction != Direction.West && !turnBlocked
    }

    def canMoveWest = {
      val blocked = direction == Direction.West && forwardMovesLeft == 0
      val turnBlocked = direction != Direction.West && forwardMovesLeft > 6
      c > 0 && !blocked && direction != Direction.East && !turnBlocked
    }

    override def compareTo(o: Node): Int = heatLoss.compareTo(o.heatLoss)

  }

  sealed trait Direction

  object Direction {
    case object East extends Direction {
      override def toString: String = ">"
    }

    case object West extends Direction {
      override def toString: String = "<"
    }

    case object North extends Direction{
      override def toString: String = "^"
    }

    case object South extends Direction{
      override def toString: String = "v"
    }
  }
}
