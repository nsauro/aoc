package y2017

import scala.io.Source

object Day12  extends App{
  val data = Source.fromResource("2017/12.data").getLines().toSeq

  val map = data.foldLeft(Map.empty[String, Seq[String]]){ case (acc, row) =>
    val parts = row.split("<->")
    acc + (parts(0).trim -> parts(1).split(",").map(_.trim))
  }

  def findConnected(nodes: Map[String, Seq[String]], queue: Set[String], count : Int) : Int = {
    if queue.isEmpty then
      count
    else
      nodes.get(queue.head) match {
        case Some(list) => findConnected(nodes.removed(queue.head), queue.tail ++ list, count + 1)
        case None => findConnected(nodes, queue.tail, count)
      }
  }

  def findGroup(nodes: Map[String, Seq[String]], queue: Set[String]): Map[String, Seq[String]] = {
    if queue.isEmpty then
      nodes
    else
      nodes.get(queue.head) match {
        case Some(list) => findGroup(nodes.removed(queue.head), queue.tail ++ list)
        case None => findGroup(nodes, queue.tail)
      }
  }

  def findAllGroups(nodes: Map[String, Seq[String]], count : Int): Int = {

    if(nodes.isEmpty) then
      count
    else
      val remaining = findGroup(nodes, Set(nodes.head._1))
      findAllGroups(remaining, count + 1)

  }
  
  println(findConnected(map, Set("0"), 0))

  println(findAllGroups(map, 0))



}
