package y2023

import org.jgrapht.alg.StoerWagnerMinimumCut
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day25 extends App {

  val data = Source.fromResource("2023/25.data").getLines().toSeq

  val graph =
    new DefaultUndirectedGraph[String, DefaultEdge](classOf[DefaultEdge])

  val nodes = mutable.HashMap.empty[String, Node]

  data.foreach { entry =>
    val ids = entry.split(":")
    val source = ids.head.trim
    val connections = ids.last.split(" ").collect {
      case s if s != "" => s.trim
    }
    if (!graph.containsVertex(source)) {
      graph.addVertex(source)
    }
    val sourceNode =
      nodes.getOrElseUpdate(source, Node(source, ListBuffer.empty))
    connections.foreach { id =>
      if (!graph.containsVertex(id)) {
        graph.addVertex(id)
      }
      graph.addEdge(source, id)
      val node = nodes.getOrElseUpdate(id, Node(id, ListBuffer.empty))
      node.connections.addOne(sourceNode)
      sourceNode.connections.addOne(node)
    }
  }

  val s = new StoerWagnerMinimumCut(graph)
  val oneSide = s.minCut().size()
  val allSides = graph.vertexSet().size()
  val otherSide = allSides - oneSide
  println(otherSide * oneSide)

  case class Node(id: String, connections: ListBuffer[Node]) {

    override def hashCode(): Int = id.hashCode

    override def equals(obj: Any): Boolean = id == obj.asInstanceOf[Node].id

    override def toString: String =
      s"id: $id -- connections: ${connections.map(_.id).mkString(",")}"
  }
}
