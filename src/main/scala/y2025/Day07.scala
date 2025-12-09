package y2025

object Day07 extends App {

  val data = scala.io.Source.fromResource("2025/7.data")
    .getLines().toArray.map(_.toArray)

  val start = data.head.indexOf('S')
  println(doPart1(data, Set(start), 0))

  val mem = scala.collection.mutable.Map.empty[(Int,Int), Long]
  println(doPart2(data, 0, start, 0))

  def doPart1(data: Array[Array[Char]], beamLocs: Set[Int], acc: Int): Int = {
    if(data.isEmpty){
      acc
    }else{
      val (newLocs, splits) = beamLocs.foldLeft((Set.empty[Int], 0)){ case((nl, ns), i) =>
        if(data.head(i) == '^'){
          (nl ++ Set(i - 1, i + 1), ns + 1)
        }else{
          (nl + i, ns)
        }
      }
      doPart1(data.tail, newLocs, acc + splits)
    }
  }


  def doPart2(data: Array[Array[Char]], depth: Int, beamLoc: Int, acc: Long): Long = {
    if(data.isEmpty){
      acc + 1
    }else if(data.head(beamLoc) == '^'){
      val leftSum = mem.getOrElse((depth + 2,beamLoc - 1), doPart2(data.tail, depth +1, beamLoc - 1, acc))
      val rightSum = mem.getOrElse((depth + 2,beamLoc + 1), doPart2(data.tail, depth +1, beamLoc + 1, acc))
      val res = acc + leftSum + rightSum
      mem.put((depth,beamLoc), res)
      res
    }else{
      doPart2(data.tail, depth + 1, beamLoc, acc)
    }

  }

}
