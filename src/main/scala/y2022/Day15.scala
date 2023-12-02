package y2022

import scala.io.Source

object Day15 extends App{

  val input = Source.fromResource("2022/Day15").getLines().toSeq

  val sensorsAndBeacons = input.foldLeft((Set.empty[SensorAndBeacon])) {
    case (acc, x) =>
      x match {
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" => {
          acc + SensorAndBeacon((sx.toInt, sy.toInt), (bx.toInt, by.toInt))
        }
      }
  }
  sensorsAndBeacons.foreach(println)



  case class SensorAndBeacon(sensor: (Int, Int), beacon: (Int, Int)) {
    val distance = manhattan(beacon)

    def manhattan(p: (Int, Int)) = {
      Math.abs(sensor._1 - p._1) + Math.abs(sensor._2 - p._2)
    }

    val minX = Math.min(sensor._1, beacon._1)
    val maxX = Math.max(sensor._1, beacon._1)

    //dis 8
    //25, 27     7 + (25 - x) == 8
    //  , 10
    //

    def isInRange(p: (Int, Int)): Boolean = {
      manhattan(p) <= distance
    }

    override def toString: String = s"$sensor -- $beacon -- $distance"
  }


  def countWhereBeaconsArent2(y : Int) : Int = {

    val points = sensorsAndBeacons.foldLeft(Set.empty[(Int, Int)]) { case (acc, sb) =>
      val range = sb.distance - Math.abs(sb.sensor._2 - y)
      if(range < 0){ // out of range
        acc
      }else{
        acc ++ ((sb.sensor._1 - range) to (sb.sensor._1 + + range)).map(x => (x, y))
      }
    }
    val beacons = sensorsAndBeacons.map(_.beacon)
    points.count(p => !beacons.contains(p))

  }

  def computePeriphery(sb : SensorAndBeacon) : LazyList[(Int, Int)] = {
    val expandedLength = sb.distance + 1
    println(s"expandedLength: $expandedLength")

    LazyList.from(0 to expandedLength).flatMap{ d => //0
      LazyList(
        (sb.sensor._1 + d, sb.sensor._2 + (expandedLength - d) ), //0 => (8, 17), -- 1 => 9, 16
        (sb.sensor._1 + d, sb.sensor._2 - (expandedLength - d) ), //0 => (8, -3)           9, -2
        (sb.sensor._1 - d, sb.sensor._2 + (expandedLength - d) ), //0 => 8, 17            7, 16
        (sb.sensor._1 - d, sb.sensor._2 - (expandedLength - d) ),  // 0 => (8, -3)         7, -2
      )
    }
  }



  //println(countWhereBeaconsArent2(2000000))

  def compute2(sb: Set[SensorAndBeacon]) : Long = {
    println(s": trying: $sb")
    val indices = computePeriphery(sb.head) // sensorsAndBeacons.flatMap(computePeriphery)

    val maybe = indices.filter(x => x._1 >= 0 && x._1 <= 4000000 && x._2 >= 0 && x._2 <= 4000000 && sensorsAndBeacons.forall(!_.isInRange(x)))
    maybe.headOption match {
      case Some(x) => x._1 * 4000000L + x._2
      case None => compute2(sb.tail)
    }
  }
  println(compute2(sensorsAndBeacons))

  //val indices = computePeriphery(sensorsAndBeacons.head)  // sensorsAndBeacons.flatMap(computePeriphery)

  //val maybe = indices.filter(x => x._1 >=0 && x._1 <= 4000000 && x._2 >=0 && x._2 <= 4000000 && sensorsAndBeacons.forall(!_.isInRange(x)))
  //println(maybe.headOption)
  //println(maybe.headOption.map(x => x._1 * 4000000 + x._2))


  //val indices = computePeriphery(SensorAndBeacon((8,7), (2,10)))
  //indices.toSeq.sorted.foreach(println)
  //println(s"amount: ${indices.size}")


}
//4184316

/*
Sensor at x=17, y=20: closest beacon is at x=21, y=22

dis = 6

21,22
x , 10

21-x + 12 = 6
21 - x = -6
 */