package y2022

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.io.Source

object Day16 extends App{


  case class Room(id : String, flowRate : Int, leadsTo : Seq[String])

  case class DistanceToRoom(distance: Int, room : Room)

  val input = Source.fromResource("2022/Day16").getLines().toSeq

  val rooms = input.map { x =>
    x.replace("valve ", "valves ").replace("tunnel ", "tunnels ").replace("lead ", "leads ") match {
      case s"Valve $id has flow rate=$rate; tunnels leads to valves $rooms" => Room(id, rate.toInt, rooms.split(",").map(_.trim))
    }
  }

  val roomMap: Map[String, Room] = rooms.map(x => (x.id -> x)).toMap

  val roomMapWithValves: Map[String, Room] = roomMap.filter(_._2.flowRate > 0)

  rooms.foreach(println)

  val aa = roomMap("AA")


  val distances : Map[Room, Set[DistanceToRoom]] = computeDistancesToRoom(roomMapWithValves.values.toSet + aa)


  def computeDistancesToRoom(roomsWithValues: Set[Room]) : Map[Room, Set[DistanceToRoom]] = {

    roomsWithValues.map{  source =>
      val dists = (roomsWithValues - source).map{ destination =>
        DistanceToRoom(distanceTo(source, destination), destination)
      }
      source -> dists
    }.toMap
  }


  def distanceTo(source: Room, destination: Room) : Int ={

    def go(r : Room, visited : Set[Room], distance : Int) : Option[Int] = {
      if(r == destination){
        Some(distance)
      }else{
        val nextRooms = r.leadsTo.map(x => roomMap(x)).filterNot(visited.contains)
        if(nextRooms.isEmpty){
          None
        }else{
          nextRooms.flatMap(n => go(n, visited + r, distance + 1)).sorted.headOption
        }
      }
    }
    go(source, Set.empty, 0).get //grody
  }




  def compute2(room : Room, turnedOnValves : Set[Room], timeRemaining : Int, pressure : Int) : Int = {

    if(turnedOnValves.size == roomMapWithValves.size){
      pressure
    }else{
      val roomDistances = distances(room).filterNot(x => turnedOnValves.contains(x.room))

      val dis = roomDistances.filterNot(x => x.distance + 1 > timeRemaining).map{ rd =>
        compute2(rd.room, turnedOnValves + rd.room, timeRemaining - (rd.distance + 1), pressure + rd.room.flowRate * (timeRemaining - (rd.distance + 1)))
      }
      if(dis.isEmpty){
        pressure
      }else{
        dis.max
      }
    }
  }

  println(compute2(aa, Set.empty, 30, 0))
  println(compute3(aa, Set.empty, roomMapWithValves.values.toSet, 30, 0))


  def compute3(room: Room, turnedOnValves: Set[Room], toVisit: Set[Room], timeRemaining: Int, pressure: Int): Int = {

    if (turnedOnValves.size == toVisit.size) {
      pressure
    } else {
      val roomDistances = distances(room).filter(x => toVisit.contains(x.room)).filterNot(x => turnedOnValves.contains(x.room))

      val dis = roomDistances.filterNot(x => x.distance + 1 > timeRemaining).map { rd =>
        compute3(rd.room, turnedOnValves + rd.room, toVisit, timeRemaining - (rd.distance + 1), pressure + rd.room.flowRate * (timeRemaining - (rd.distance + 1)))
      }
      if (dis.isEmpty) {
        pressure
      } else {
        dis.max
      }
    }
  }



  val roomsWithValues = roomMapWithValves.values.toSet
  val allSubsets = roomsWithValues.subsets()

  import scala.concurrent.ExecutionContext.Implicits.global

  val x: Future[Iterator[Int]] = Future.sequence{
    allSubsets.map { sub =>
      Future{
        val guy = compute3(aa, Set.empty, sub, 26, 0)
        val elephant = compute3(aa, Set.empty, roomsWithValues -- sub, 26, 0)
        guy + elephant
      }
    }
  }

  val res = x.map(_.max)
  val actualRes = Await.result(res, 4.hours)
  println(actualRes)

  /*

  val maybe = allSubsets.map{ sub =>
    val guy = compute3(aa, Set.empty, sub, 26, 0)
    val elephant = compute3(aa, Set.empty, roomsWithValues -- sub, 26, 0)
    guy + elephant
  }.max

  println(maybe)
*/

}
