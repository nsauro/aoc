package y2017

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day07 extends App:

  case class Node2(
      id: String,
      weight: Int,
      children: ListBuffer[Node2] = ListBuffer.empty
  ) {

    lazy val totalWeight: Int =
      if (children.isEmpty) then weight
      else weight + children.map(_.totalWeight).sum
  }

  val data = Source.fromResource("2017/7.data").getLines().toSeq
  val nodeR = """(\w+) \((\d+)\)( -> (.+))?""".r

  // create all nodes no kids
  val nodes = data.foldLeft(Map.empty[String, Node2]) {
    case (acc, nodeR(id, weight, _, _)) => acc + (id -> Node2(id, weight.toInt))
    case (acc, _)                       => acc
  }

  data.foreach {
    case nodeR(id, weight, _, childrenOpt) => {
      Option(childrenOpt).foreach { children =>
        children.split(",").map(_.trim).filterNot(_ == "").foreach { childId =>
          nodes(id).children.append(nodes(childId))
        }
      }
    }
    case _ =>
  }

  val rootNodeName =
    nodes.keys.find(x => nodes.values.forall(!_.children.exists(_.id == x)))

  println(rootNodeName)
  val rootNode = nodes(rootNodeName.get)

  println(doTheThing(rootNode, 0))

  def doTheThing(node: Node2, diff: Int): Int = {
    val byTotalWeights = node.children.groupBy(_.totalWeight)
    byTotalWeights.find(_._2.size == 1).match {
      case Some((totalWeight, ListBuffer(off))) => {
        val otherWeight = byTotalWeights.find(_._2.size > 1).get._1
        val diff = otherWeight - totalWeight
        doTheThing(off, diff)
      }
      case _ => { // children are balanced
        node.weight + diff
      }
    }
  }
