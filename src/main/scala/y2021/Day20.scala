package y2021

import scala.io.Source

object Day20 extends App {

  val data = Source.fromResource("2021/20.data").getLines()

  val alg = data.next().toCharArray

  data.next() // skip blank

  val image: Vector[Vector[Char]] = Vector.from(data.toSeq.map(_.toVector))

  val maxTimes = 50

  val enhanced1x = enhance(image, 0)

  val pixelCount = enhanced1x.map(_.count(_ == '#')).sum
  println(pixelCount)

  printGrid(enhanced1x)

  def enhance(
      input: Vector[Vector[Char]],
      timesExecuted: Int
  ): Vector[Vector[Char]] = {

    // printGrid(input)
    val expansionChar = if (timesExecuted % 2 == 0) '.' else '#'

    if (timesExecuted == maxTimes) {
      input
    } else {

      val expanded = grow(input, expansionChar)
      // println("-------EXPANDED--------")
      // printGrid(expanded)
      val res = expanded.zipWithIndex.map { case (row, r) =>
        row.zipWithIndex.map { case (_, c) =>
          alg(getAlgIndex(expanded, r, c, expansionChar))
        }
      }
      enhance(res, timesExecuted + 1)
    }
  }

  def grow(input: Vector[Vector[Char]], char: Char): Vector[Vector[Char]] = {

    val paddedCols = input.map(r => char +: r :+ char)
    val newRowSize = paddedCols.head.size
    val newRow = char.toString.padTo(newRowSize, char).toVector
    newRow +: paddedCols :+ newRow

  }

  def mapToBinary(c: Char): Char = if (c == '.') '0' else '1'

  def lookupValue(
      image: Vector[Vector[Char]],
      r: Int,
      c: Int,
      char: Char
  ): Char = {

    if (r < 0 || r >= image.size || c < 0 || c >= image.head.length) {
      mapToBinary(char)
    } else {
      mapToBinary(image(r)(c))
    }
  }

  def getAlgIndex(image: Vector[Vector[Char]], r: Int, c: Int, char: Char) = {

    val thing: Array[Char] = Array(
      lookupValue(image, r - 1, c - 1, char), // top left
      lookupValue(image, r - 1, c, char), // top
      lookupValue(image, r - 1, c + 1, char), // top right
      lookupValue(image, r, c - 1, char), // left
      lookupValue(image, r, c, char), // middle
      lookupValue(image, r, c + 1, char), // right
      lookupValue(image, r + 1, c - 1, char), // bottom left
      lookupValue(image, r + 1, c, char), // bottom
      lookupValue(image, r + 1, c + 1, char) // bottom right
    )

    Integer.parseInt(new String(thing), 2)

  }

  def printGrid(input: Vector[Vector[Char]]): Unit = {
    println(
      s"-----------${input.size}  x ${input.head.size} ---------------------"
    )
    input.foreach(x => println(x.mkString("")))
  }

}
