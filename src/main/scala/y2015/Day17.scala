package y2015

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day17 extends App{


  val coins = Source.fromResource("2015/17.data").getLines().map(_.toInt).toVector

  val hacked = coins.zipWithIndex





  println(combos(hacked, 150))

  def combos(c : Vector[(Int, Int)], amount : Int) : Int = {

    val x = for{
      s <- 1 to c.size
    } yield {
      (s, c.combinations(s).count(x => x.map(_._1).sum == amount))
    }

    x.foreach(println)
    val bla =x.groupMapReduce(_._1)((_._2))(_ + _)

    bla(bla.keySet.min)


  }

}
