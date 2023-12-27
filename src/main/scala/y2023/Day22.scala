package y2023

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day22 extends App{

  val ShapeParams = raw"""(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)""".r

  val data = Source.fromResource("2023/22.data").getLines()

  //sorted by lowest z
  val shapes: Seq[(Shape, Int)] = data.zipWithIndex.map{
    case (ShapeParams(x1, y1, z1, x2, y2, z2), index) => {
      val minZ = Math.min(z1.toInt, z2.toInt)
      (Shape(index, (x1.toInt,y1.toInt), (x2.toInt,y2.toInt), (z2.toInt - z1.toInt).abs + 1, false), minZ)
    }
  }.toSeq.sortBy(_._2)

  //condense
  val initialLayers = shapes.foldLeft(mutable.HashMap.empty[Int, ListBuffer[Shape]]){ case (layers,(shape, level)) =>
    //println(s"finding home for $shape -- $level")
    val (updated, _) = condenseShape(shape, level, layers)
    updated
  }

  val initialMap = initialLayers.flatMap{ case (i, pieces) =>
    pieces.collect {
      case p if !p.isVerticalStub => (p,i)
    }
  }

  val res = shapes.map{ case(shape, _) =>
    val removed = shapes.filterNot(_._1.id  == shape.id)
    val layers = removed.foldLeft(mutable.HashMap.empty[Int, ListBuffer[Shape]]) { case (layers, (shape, level)) =>
      //println(s"finding home for $shape -- $level")
      val (updated, _) = condenseShape(shape, level, layers)
      updated
    }
    val layerMap = layers.flatMap { case (i, pieces) =>
      pieces.collect {
        case p if !p.isVerticalStub => (p,i)
      }
    }
    layerMap.count{ case (s,i) => initialMap(s) != i}
  }.sum

  println(res)
  println(countDisintegrateableBricks(initialLayers))


  def condenseShape(shape : Shape, level: Int, layers: mutable.HashMap[Int, ListBuffer[Shape]]): (mutable.HashMap[Int, ListBuffer[Shape]], Int) ={
    val layerI = findLayer(shape, Math.max(1, level - 1), layers)
    val layer = layers.getOrElse(layerI, ListBuffer.empty[Shape])
    layer.addOne(shape)
    layers.put(layerI, layer)
    if (shape.height > 1) { //add stubs
      (layerI + 1 until layerI + shape.height).foreach { i =>
        //println("making stub")
        val layer = layers.getOrElse(i, ListBuffer.empty[Shape])
        layer.addOne(shape.copy(isVerticalStub = true))
        layers.put(i, layer)
      }
    }
    (layers, layerI)
  }
  def countDisintegrateableBricks(layers: mutable.HashMap[Int, ListBuffer[Shape]]) : Int = {
    layers.keys.toSeq.sorted.map{ layer =>
      if(layer == layers.keys.max){ //top most layer can all disintegrate
        layers(layer).filterNot(_.isVerticalStub).size
      }else{
        layers(layer).filterNot(_.isVerticalStub).count { shape =>
          val layerAbove = layer + shape.height
          if(layerAbove > layers.keys.max){
            true //handle case where a vertical bar is close to the top
          }else{
            val layerToCheck = layers(layerAbove)
            val overlaps = layerToCheck.filter(_.overlaps(shape)) //all shapes which overlap current
            val layerWithoutShape = layers(layerAbove - 1).filterNot(_.sameWithoutStub(shape))
            //check overlaps again..make sure
            val stillOverlaps = overlaps.count(x => layerWithoutShape.exists(_.overlaps(x)))
            overlaps.size == stillOverlaps//nothing lost support
          }
        }
      }
    }.sum
  }

  def findLayer(shape : Shape, index: Int, layers: mutable.HashMap[Int, ListBuffer[Shape]]) : (Int) = {
    if(index  == 0){
      index + 1
    }else{
      val layer = layers.getOrElse(index, ListBuffer.empty[Shape])
      if(layer.forall(!_.overlaps(shape))){
        findLayer(shape, index - 1, layers)
      }else{
        index + 1
      }
    }
  }


  case class Shape(id:Int, firstBlock : (Int, Int), lastBlock : (Int, Int), height: Int, isVerticalStub: Boolean) {
    val blocks = for{
      x <- firstBlock._1 to lastBlock._1
      y <- firstBlock._2 to lastBlock._2
    } yield{
      (x,y)
    }

    override val toString: String = {
      s"${blocks.mkString(",")} -- $height -- $isVerticalStub}"
    }

    def sameWithoutStub(shape: Shape) = id == shape.id

    def overlaps(other: Shape) = this.blocks.exists(other.blocks.contains)
  }

}
