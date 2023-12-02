package y2022

import scala.io.Source

object Day06 extends App{

  val data = Source.fromResource("2022/Day06").getLines().toSeq

  val testStrings = Seq(
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )

  testStrings.map(getMarkerIndex(_, 4)).foreach(println)

  data.map(getMarkerIndex(_, 4)).foreach(println)


  testStrings.map(getMarkerIndex(_, 14)).foreach(println)

  data.map(getMarkerIndex(_, 14)).foreach(println)


  def getMarkerIndex(str : String, size : Int) = {
    str.sliding(size).toSeq.zipWithIndex.collectFirst{
      case (str, i) if (str.toSet.size) == size => (str, i + size)
    }
  }

}
