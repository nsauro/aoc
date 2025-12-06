package y2025

object Day04 extends App {

  val data = scala.io.Source.fromResource("4.data").getLines().map(_.toCharArray).toArray

  val rows = 0 until data.size
  val columns = 0 until data.head.size

  val part1 = (for {
    r <- rows
    c <- columns if data(r)(c) == '@'
  } yield{
    val res = (for {
      a <- -1 to 1 if (r+a) >= 0 && (r+a) < data.size
      b <- -1 to 1 if (c+b) >= 0 && (c+b) < data.head.size && !(a == 0 && b == 0) && data(r + a)(c + b) == '@'
    } yield {
      1
    }).sum
    if(res < 4) 1 else 0
  }).sum
  println(part1)


  val part2 = removeAll(data, 0)
  println(part2)

  def removeAll(tp: Array[Array[Char]], removed: Long) : Long = {
    val toRemove = (for {
      r <- rows
      c <- columns if data(r)(c) == '@'
    } yield {
      val res = (for {
        a <- -1 to 1 if (r + a) >= 0 && (r + a) < data.size
        b <- -1 to 1 if (c + b) >= 0 && (c + b) < data.head.size && !(a == 0 && b == 0) && data(r + a)(c + b) == '@'
      } yield {
        1
      }).sum
      Option.when(res < 4) (r,c)
    }).flatten

    if(toRemove.isEmpty){
      removed
    }else{
      toRemove.foreach({case(r,c) => tp(r)(c) = '.'})
      removeAll(tp, toRemove.size + removed)
    }
  }

}
