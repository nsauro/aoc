package y2015

import scala.io.Source

object Day6 extends App{

  val data = Source.fromResource("2015/6.data").getLines().toSeq


  val On = raw"""turn on (\d+),(\d+) through (\d+),(\d+)""".r
  val Off = raw"""turn off (\d+),(\d+) through (\d+),(\d+)""".r
  val Toggle = raw"""toggle (\d+),(\d+) through (\d+),(\d+)""".r


  /*val results = turnOnLights(data, Set.empty)

  println(results.size)*/

  println(turnOnLights2(data, Map.empty))

  def turnOnLights(instructions : Seq[String], onLights : Set[(Int, Int)]) : Set[(Int, Int)] = {

    if(instructions.isEmpty){
      onLights
    }else{
      instructions.head match {
        case On(r1, c1, r2,c2)  => turnOnLights(instructions.tail, onLights ++ createLights(r1.toInt,c1.toInt,r2.toInt,c2.toInt))
        case Off(r1, c1, r2,c2)  =>turnOnLights(instructions.tail, onLights -- createLights(r1.toInt,c1.toInt,r2.toInt,c2.toInt))
        case Toggle(r1, c1, r2,c2)  =>{
          val toggleLights = createLights(r1.toInt,c1.toInt,r2.toInt,c2.toInt)
          val (toRemove, toAdd) = toggleLights.partition(onLights.contains)
          val updatedLights = (onLights -- toRemove) ++ toAdd
          turnOnLights(instructions.tail, updatedLights)
        }
      }
    }
  }

  def turnOnLights2(instructions : Seq[String], onLights : Map[(Int, Int), Int]) : Int = {

    if(instructions.isEmpty){
      onLights.values.sum
    }else{
      instructions.head match {
        case On(r1, c1, r2,c2)  =>
          turnOnLights2(instructions.tail, updateMap(onLights, createLights(r1.toInt,c1.toInt,r2.toInt,c2.toInt), 1))
        case Off(r1, c1, r2,c2)  =>
          turnOnLights2(instructions.tail, updateMap(onLights, createLights(r1.toInt,c1.toInt,r2.toInt,c2.toInt), -1))
        case Toggle(r1, c1, r2,c2)  =>{
          turnOnLights2(instructions.tail, updateMap(onLights, createLights(r1.toInt,c1.toInt,r2.toInt,c2.toInt), 2))
        }
      }
    }
  }

  def updateMap(onLights : Map[(Int, Int), Int], newLights : Set[(Int,Int)], intensity : Int) : Map[(Int, Int), Int] = {

    newLights.foldLeft(onLights){ case (acc, light) =>
      val currentIntensity = acc.getOrElse(light, 0)
      val updatedIntensity = currentIntensity + intensity
      if(updatedIntensity <= 0){
        acc - light
      }else{
        acc + (light -> updatedIntensity)
      }
    }
  }



  def createLights(r1 : Int, c1: Int, r2: Int, c2:Int) : Set[(Int, Int)] = {

    val res = for{
      r <- r1 to r2
      c <- c1 to c2
    } yield{
      (r,c)
    }

    res.toSet

  }

}
