package y2023

import java.util.PriorityQueue
import scala.io.Source

object Day17 extends App {

  val data = Source.fromResource("2023/17.data").getLines().toArray.map(_.toArray.map(x => Node(x.asDigit, Int.MaxValue, None, None)))


  data.foreach{
    x => println(x.map(_.cost).mkString(""))
  }
  println("------------------")

  val view = data.map{ x =>
    Array.from(x.map(_.cost.toString))
  }

  def updateView(r : Int, c: Int) : Unit = {
    val node = data(r)(c)
    val str = node.direction.map(_.toString).getOrElse("-")
    view(r)(c) = str
    node.previous match{
      case Some((r1, c1)) => updateView(r1, c1)
      case None => ()
    }

  }

  dijktras(0,0)

  println(data.last.last)

  updateView(data.length - 1, data.head.length -1)

  view.foreach {
    x => println(x.mkString(""))
  }



  def dijktras(r: Int, c: Int) : Unit = {

    val q = new PriorityQueue[NodeInFlight]()
    data(r)(c) = data(r)(c).copy(heatLoss = 0)
    q.add(NodeInFlight(r, c, None, 3, None))

    while(q.size() != 0){
      val cur = q.poll()

      val node = data(cur.r)(cur.c)
      data(cur.r)(cur.c) = node.copy(direction = cur.direction, previous = cur.prev.map(x => (x.r, x.c)))
      val newMin = node.heatLoss + node.cost
      if(cur.canMoveEast && data(cur.r)(cur.c + 1).heatLoss > newMin){
        q.remove(NodeInFlight(cur.r, cur.c + 1, None, -1, None))
        data(cur.r)(cur.c + 1) = data(cur.r)(cur.c + 1).copy(heatLoss = newMin)
        q.add(NodeInFlight(cur.r, cur.c + 1,Some(Direction.East), cur.direction.fold(2)(x => if(x == Direction.East) cur.forwardMovesLeft - 1 else 2), Some(cur)))
      }

      if (cur.canMoveWest && data(cur.r)(cur.c - 1).heatLoss > newMin) {
        q.remove(NodeInFlight(cur.r, cur.c - 1, None, -1, None))
        data(cur.r)(cur.c - 1) = data(cur.r)(cur.c - 1).copy(heatLoss = newMin)
        q.add(NodeInFlight(cur.r, cur.c - 1, Some(Direction.West), cur.direction.fold(2)(x => if (x == Direction.West) cur.forwardMovesLeft - 1 else 2), Some(cur)))
      }

      if (cur.canMoveNorth && data(cur.r - 1)(cur.c ).heatLoss > newMin) {
        q.remove(NodeInFlight(cur.r - 1, cur.c, None, -1, None))
        data(cur.r - 1)(cur.c ) = data(cur.r - 1)(cur.c).copy(heatLoss = newMin)
        q.add(NodeInFlight(cur.r - 1, cur.c, Some(Direction.North), cur.direction.fold(2)(x => if (x == Direction.North) cur.forwardMovesLeft - 1 else 2), Some(cur)))
      }

      if (cur.canMoveSouth && data(cur.r + 1)(cur.c).heatLoss > newMin) {
        q.remove(NodeInFlight(cur.r + 1, cur.c, None, -1, None))
        data(cur.r + 1)(cur.c ) = data(cur.r + 1)(cur.c).copy(heatLoss = newMin)
        q.add(NodeInFlight(cur.r + 1, cur.c, Some(Direction.South), cur.direction.fold(2)(x => if (x == Direction.South) cur.forwardMovesLeft - 1 else 2), Some(cur)))
      }
    }
  }

  case class Node(cost: Int, heatLoss: Int, direction: Option[Direction], previous : Option[(Int, Int)])

  case class NodeInFlight(r : Int, c: Int, direction: Option[Direction], forwardMovesLeft : Int, prev: Option[NodeInFlight]) extends Comparable[NodeInFlight]{


    def canMoveNorth = r > 0 && direction.forall{x =>
      val blocked = (x == Direction.North && forwardMovesLeft == 0)
      x != Direction.South && !blocked
    }

    def canMoveSouth = r < data.length - 1 && direction.forall { x =>
      val blocked = (x == Direction.South && forwardMovesLeft == 0)
      x != Direction.North && !blocked
    }

    def canMoveEast = c < data.head.length - 1 && direction.forall { x =>
      val blocked = (x == Direction.East && forwardMovesLeft == 0)
      x != Direction.West && !blocked
    }

    def canMoveWest = c > 0 && direction.forall { x =>
      val blocked = (x == Direction.West && forwardMovesLeft == 0)
      x != Direction.East && !blocked
    }

    override def compareTo(o: NodeInFlight): Int = data(r)(c).heatLoss.compareTo(data(o.r)(o.c).heatLoss)

    override def equals(obj: Any): Boolean = {
      val n = obj.asInstanceOf[NodeInFlight]
      r == n.r && c == n.c
    }
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
