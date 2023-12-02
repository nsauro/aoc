package y2021

import scala.io.Source
import scala.util.Random

object Day19 extends App {


  val data = Source.fromResource("2021/19.data").getLines().toSeq

  val ScannerHeader = raw"--- scanner (\d+) ---".r
  val BeaconInput = raw"([\d\-]+),([\d\-]+),([\d\-]+)".r

  val scanners = data.foldLeft(Vector.empty[Scanner]) { case (acc, line) =>
    line match {
      case ScannerHeader(id) => acc :+ Scanner(id.toInt, Set.empty)
      case BeaconInput(x, y, z) => acc.init :+ acc.last.copy(beacons = acc.last.beacons + Beacon(x.toInt, y.toInt, z.toInt))
      case _ => acc

    }
  }

  val res = processScanners(ProcessResults(scanners.head, Seq.empty), scanners.tail)

  println(res.aggregatedScanner)
  println(res.aggregatedScanner.beacons.size)
  res.allScannerLocations.foreach(println)

  println(computeLargestDistance(res.allScannerLocations))


  def computeLargestDistance(locations: Seq[Beacon]) = {
    val distances = for {
      f <- locations
      s <- locations
    } yield {
      Seq(
        f.computeDistance(s),
        s.computeDistance(f)
      )
    }

    distances.flatten.toSet.max
  }


  def processScanners(combined: ProcessResults, remaining: Seq[Scanner]): ProcessResults = {

    println(s"combined scanner has ${combined.aggregatedScanner.beacons.size} beacons")
    println(s"remaining scanners: ${remaining.size} - ${remaining.map(_.id).mkString(",")} ")
    /*  println(s"x: ${combined.minX} = ${combined.maxX}")
    println(s"y: ${combined.minY} = ${combined.maxY}")
    println(s"z: ${combined.minZ} = ${combined.maxZ}")*/

    if (remaining.isEmpty) {
      combined
    } else {

      val (newC, newRemaining, newFoundScanners) = remaining.foldLeft((combined.aggregatedScanner, Seq.empty[Scanner], Seq.empty[Beacon])) { case ((acc, unmapped, foundScanners), scanner) =>
        checkOverlap(acc, scanner) match {
          case Some(res) => {
            println(s"scanner: ${scanner.id} overlaps.. it's coords are: ${res.diff}")
            val newAcc = acc.copy(beacons = acc.beacons ++ res.mappedBeacons)
            //println(newAcc)
            (newAcc, unmapped, foundScanners :+ res.diff)
          }
          case None => {
            println(s"scanner: ${scanner.id} does not match")
            (acc, unmapped :+ scanner, foundScanners)
          }
        }
      }
      processScanners(combined.copy(aggregatedScanner = newC, combined.allScannerLocations ++ newFoundScanners), Random.shuffle(newRemaining))
    }
  }

  def checkOverlap(first: Scanner, second: Scanner): Option[OverlapResults] = {

    object Overlaps {
      def unapply(sc: Scanner): Option[OverlapResults] = first.overlap2(sc)
    }

    val rotated: Seq[Scanner] = Rotations.rotations.flatMap(_.rotate(second))
    rotated.collectFirst {
      case Overlaps(res) => res
    }
  }


  case class Rotation(sideRotationF: Scanner => Scanner) {


    def rotate(scanner: Scanner): Seq[Scanner] = {

      val sideRotation = sideRotationF(scanner)
      Seq(
        sideRotation,
        sideRotation.copy(beacons = sideRotation.beacons.map(faceRotate1)),
        sideRotation.copy(beacons = sideRotation.beacons.map(faceRotate2)),
        sideRotation.copy(beacons = sideRotation.beacons.map(faceRotate3))
      )
    }

    def faceRotate1(beacon: Beacon): Beacon = {
      beacon.copy(x = beacon.y * -1, y = beacon.x)
    }

    def faceRotate2(beacon: Beacon): Beacon = {
      beacon.copy(x = beacon.x * -1, y = beacon.y * -1)
    }

    def faceRotate3(beacon: Beacon): Beacon = {
      beacon.copy(x = beacon.y, y = beacon.x * -1)

    }
  }

  object Rotations {
    val rotations = Seq(
      Rotation(rotate1),
      Rotation(rotate2),
      Rotation(rotate3),
      Rotation(rotate4),
      Rotation(rotate5),
      Rotation(rotate6),
    )

    def rotate1(scanner: Scanner): Scanner = {
      scanner.copy(beacons = scanner.beacons.map(b => b.copy(x = b.z * -1, z = b.x)))
    }

    def rotate2(scanner: Scanner): Scanner = {
      scanner.copy(beacons = scanner.beacons.map(b => b.copy(x = b.x * -1, z = b.z * -1)))
    }

    def rotate3(scanner: Scanner): Scanner = {
      scanner.copy(beacons = scanner.beacons.map(b => b.copy(x = b.z, z = b.x * -1)))
    }

    def rotate4(scanner: Scanner): Scanner = {
      scanner.copy(beacons = scanner.beacons.map(b => b.copy(y = b.z * -1, z = b.y)))
    }

    def rotate5(scanner: Scanner): Scanner = {
      scanner.copy(beacons = scanner.beacons.map(b => b.copy(y = b.z, z = b.y * -1)))
    }

    def rotate6(scanner: Scanner): Scanner = scanner
  }


  case class Beacon(x: Int, y: Int, z: Int) {
    override def toString: String = s"($x,$y,$z)"

    def mapToCoords(newX: Int, newY: Int, newZ: Int) = {
      this.copy(x + newX, y + newY, z + newZ)
    }

    def computeDistance(other: Beacon): Int = {
      (this.x - other.x).abs + (this.y - other.y).abs + (this.z - other.z).abs
    }
  }

  case class Scanner(id: Int, beacons: Set[Beacon]) {
    lazy val maxX = beacons.maxBy(_.x).x
    lazy val minX = beacons.minBy(_.x).x
    lazy val maxY = beacons.maxBy(_.y).y
    lazy val minY = beacons.minBy(_.y).y
    lazy val maxZ = beacons.maxBy(_.z).z
    lazy val minZ = beacons.minBy(_.z).z

    lazy val xRange = minX - 2000 to maxX + 2000
    lazy val yRange = minY - 2000 to maxY + 2000
    lazy val zRange = minZ - 2000 to maxZ + 2000


    lazy val allXs: Map[Int, Int] = beacons.toSeq.map(_.x).groupMapReduce(identity)(_ => 1)(_ + _)


    override def toString: String = {
      s"""
         |--- scanner $id

        | ${beacons.toSeq.sortBy(_.x).map(_.toString).mkString("\n")}

        |
        |""".stripMargin
    }


    def overlap2(scanner : Scanner) : Option[OverlapResults] = {


      object MatchesY {def unapplySeq(x : Int) :Option[Seq[(Int, Int)]] = {

          val res = yRange.filter(y => checkYs(scanner, x, y))
          if(res.isEmpty){
            None
          }else{
            Some(res.map((x, _)))
          }
        }
      }

      object MatchesZ{

        def unapplySeq(xy: (Int, Int)) :Option[Seq[(Int, Int, Int)]] = {

          val res = zRange.filter(z => checkZs(scanner, xy._1, xy._2, z))
          if(res.isEmpty){
            None
          }else{
            Some(res.map((xy._1, xy._2, _)))
          }
        }
      }

      object MatchesScanner{
        def unapply(xyz: (Int, Int,Int)) : Option[OverlapResults] = {
          val mappedBeacons = scanner.beacons.map(b => b.copy(x =b.x + xyz._1, y = b.y + xyz._2, z = b.z + xyz._3))

          val re = Option(beacons.intersect(mappedBeacons)).filter(_.size >= 12)
          re.map{ m =>
            OverlapResults(m, Beacon(xyz._1, xyz._2, xyz._3), mappedBeacons)
          }
        }
      }



      val


      matchingXs = xRange.filter(x => checkXs(scanner, x))

      val matchingXYs = matchingXs.collect{
        case MatchesY(ys) => ys
      }

      val matchingXYZs: Seq[(Int, Int, Int)] = matchingXYs.collect{
        case MatchesZ(zs) => zs
      }

      matchingXYZs.collectFirst{
        case MatchesScanner(r) => r
      }
    }

    def checkXs(scanner : Scanner, x :Int)  : Boolean = {
      val mappedXs = scanner.beacons.toSeq.map(_.x + x)
      checkMatches(mappedXs, allXs)
    }

    def checkYs(scanner : Scanner, x :Int, y :Int)  : Boolean = {
      val mappedYs = scanner.beacons.toSeq.map(b => (b.x + x, b.y + y))
      val xyCounts: Map[(Int, Int), Int] = beacons.toSeq.map(b => (b.x, b.y)).groupMapReduce(identity)(_ => 1)(_ + _)
      checkMatches(mappedYs, xyCounts)

    }

    def checkZs(scanner : Scanner, x : Int, y : Int, z: Int)  : Boolean  = {
      val mappedZs = scanner.beacons.map(b => b.copy(x =b.x + x, y = b.y + y, z = b.z + z))
      beacons.intersect(mappedZs).size >= 12

    }

    def checkMatches[T](ts : Seq[T], thisCount: Map[T, Int]) : Boolean = {
      val (_, matches) =ts.foldLeft((collection.mutable.Map.from(thisCount), 0)){ case ((counts, hits ), mx) =>{
        val hasX = counts.get(mx)
        hasX match {
          case None => (counts, hits)
          case Some(x) => {
            val updated = x - 1
            if(updated == 0){
              counts.remove(mx)
              (counts, hits + 1)
            }else{
              counts.update(mx, updated)
              (counts, hits + 1)
            }

          }
        }
      }
    }
      matches >= 12
  }
    }


  case class ProcessResults(aggregatedScanner : Scanner, allScannerLocations: Seq[Beacon])

  case class OverlapResults(overlappingBeacons : Set[Beacon], diff : Beacon, mappedBeacons : Set[Beacon])

}
