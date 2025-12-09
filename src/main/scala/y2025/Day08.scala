package y2025

import scala.collection.mutable

object Day08 extends App {

  val data = scala.io.Source.fromResource("8.data").getLines()
    .map(x =>
      val y = x.split(",").map(_.toInt)
      (y(0), y(1), y(2))
    ).toSeq

  val pairs = (for {
    i <- data
    j <- data if (i != j)
  } yield {
    (Set(i,j), dis(i,j))
  })
    .distinctBy(_._1)
    .sortBy(_._2)
    .map(x => (x._1.head, x._1.last, x._2))


  println(makeConnections(pairs, 0, Set.empty))
  println(makeAllConnections(pairs, Set.empty, 0L))


  def makeConnections(p : Seq[((Int, Int, Int), (Int, Int, Int), Double)],
                      connectionsMade: Int,
                      acc: Set[mutable.HashSet[(Int, Int, Int)]]) : Long = {

    if(connectionsMade == 1000){
      val sorted = acc.toSeq.sortBy(-_.size)
      sorted.map(_.size).take(3).reduce(_ * _)
    }else{

      val (toJoin, others) = acc.partition(x => x.contains(p.head._1) || x.contains(p.head._2))

      if(toJoin.isEmpty){ //new nodes, new circuit
        val newSet = mutable.HashSet(p.head._1, p.head._2)
        val updatedAcc = others + newSet
        makeConnections(p.tail, connectionsMade + 1, updatedAcc)
      }else if(toJoin.exists(x => x.contains(p.head._1) && x.contains(p.head._2))) { //already contained
        makeConnections(p.tail, connectionsMade + 1, acc)
      }else{ // we hae overlaps..merge and connect
        val x = mutable.HashSet.from(toJoin.flatten ++ Set(p.head._1, p.head._2))
        makeConnections(p.tail, connectionsMade + 1, others + x)
      }
    }
  }


  def makeAllConnections(p: Seq[((Int, Int, Int), (Int, Int, Int), Double)],
                         acc: Set[mutable.HashSet[(Int, Int, Int)]],
                         coords: Long): Long = {

    if(p.isEmpty){
      coords
    }else{
      val (toJoin, others) = acc.partition(x => x.contains(p.head._1) || x.contains(p.head._2))

      if (toJoin.isEmpty) { //new nodes, new circuit
        val newSet = mutable.HashSet(p.head._1, p.head._2)
        val updatedAcc = others + newSet
        makeAllConnections(p.tail, updatedAcc, coords)
      } else if (toJoin.exists(x => x.contains(p.head._1) && x.contains(p.head._2))) { //already contained
        makeAllConnections(p.tail, acc, coords)
      } else { // we hae overlaps..merge and connect
        val x = mutable.HashSet.from(toJoin.flatten ++ Set(p.head._1, p.head._2))
        val updatedAcc = others + x
        val newCoords = if(updatedAcc.size == 1){
          (p.head._1._1.toLong * p.head._2._1.toLong)
        } else coords
        makeAllConnections(p.tail, updatedAcc, newCoords)
      }
    }
  }

  def dis(p: (Int, Int, Int), q: (Int, Int, Int)): Double = {
    val x = Math.pow(p._1 - q._1,2)
    val y = Math.pow(p._2 - q._2,2)
    val z = Math.pow(p._3 - q._3,2)
    Math.sqrt(x + y + z)
  }
}
