package y2016

import scala.io.Source

object Day21 extends App{

  val SwapPosition = """swap position (\d+) with position (\d+)""".r
  val SwapLetter = """swap letter ([a-z]) with letter ([a-z])""".r
  val RotateLeft = """rotate left (\d+) .*""".r
  val RotateRight = """rotate right (\d+) .*""".r
  val RotateFrom = """rotate based on position of letter ([a-z])""".r
  val Reverse = """reverse positions (\d+) through (\d+)""".r
  val Move = """move position (\d+) to position (\d+)""".r


  val data = Source.fromResource("2016/21.data").getLines().toSeq

  val res = compute("abcdefgh".toCharArray, data)
  println("--------------------------------------------------")
  val reverse = computeReverse("fbgdceah".toCharArray(), data.reverse)
  //println(compute("abcdefgh".toCharArray, data).mkString(""))
  println(s"res: ${res.mkString}")
  println(s"reverse: ${reverse.mkString}")


  def compute(str: Array[Char], instructions: Seq[String]): Array[Char] = {

    if(instructions.isEmpty){
      str
    }else{
      println(s"${str.mkString("")} -- next up: -- ${instructions.head}" )
      val res = instructions.head match {
        case SwapPosition(from, to) => {
          val fromI = from.toInt
          val toI = to.toInt
          swap(str, fromI, toI)
        }
        case SwapLetter(x, y) => {
          val fromI = str.indexOf(x.head)
          val toI = str.indexOf(y.head)
          swap(str, fromI, toI)
        }
        case RotateLeft(x) => {
          rotateLeft(str, x.toInt)
        }
        case RotateRight(x) => {
          rotateRight(str, x.toInt)
        }
        case RotateFrom(x) => {
          rotateFrom(str, x.head)
        }

        case Reverse(x, y) => {
          val xi = x.toInt
          val yi = y.toInt
          val sub = str.slice(xi, yi + 1).reverse
          replace(str, xi, sub)
        }

        case Move(x, y) => {
          val xi = x.toInt
          val yi = y.toInt
          if(xi < yi){ //move left
            val sub = str.slice(xi, yi + 1)
            val rotated = rotateLeft(sub, 1)
            replace(str, xi, rotated)
          }  else {
            val sub = str.slice(yi, xi + 1)
            val rotated = rotateRight(sub, 1)
            replace(str, yi, rotated)
          }
        }
      }
      compute(res, instructions.tail)
    }
  }


  def computeReverse(str: Array[Char], instructions: Seq[String]): Array[Char] = {
    if (instructions.isEmpty) {
      str
    } else {
      println(s"${str.mkString("")} -- next up: -- ${instructions.head}")
      val res = instructions.head match {
        case SwapPosition(from, to) => {
          val fromI = from.toInt
          val toI = to.toInt
          swap(str, fromI, toI)
        }
        case SwapLetter(x, y) => {
          val fromI = str.indexOf(x.head)
          val toI = str.indexOf(y.head)
          swap(str, fromI, toI)
        }
        case RotateLeft(x) => {
          rotateRight(str, x.toInt)
        }
        case RotateRight(x) => {
          rotateLeft(str, x.toInt)
        }
        case RotateFrom(x) => {
          unrotate(str, x.head)
        }

        case Reverse(x, y) => {
          val xi = x.toInt
          val yi = y.toInt
          val sub = str.slice(xi, yi + 1).reverse
          replace(str, xi, sub)
        }

        case Move(x, y) => {
          val xi = x.toInt
          val yi = y.toInt
          if (xi < yi) { //move left
            val sub = str.slice(xi, yi + 1)
            val rotated = rotateRight(sub, 1)
            replace(str, xi, rotated)
          } else {
            val sub = str.slice(yi, xi + 1)
            val rotated = rotateLeft(sub, 1)
            replace(str, yi, rotated)
          }
        }
      }
      computeReverse(res, instructions.tail)
    }
  }






  def swap(str: Array[Char], x : Int, y: Int) : Array[Char] = {
    val f = str(x)
    val t = str(y)
    str(x) = t
    str(y) = f
    str
  }

  def rotateRight(str: Array[Char], times: Int) : Array[Char] = {
    val bla = (0 until times).foldLeft(str.reverse) { case (acc, _) =>
      val s = acc.toSeq
      (s.tail :+ s.head).toArray
    }
    bla.reverse
  }

  def rotateLeft(str: Array[Char], times: Int) : Array[Char] = {
    val res = (0 until times).foldLeft(str) { case (acc, _) =>
      val s = acc.toSeq
      (s.tail :+ s.head).toArray
    }
    res
  }



  def replace(str: Array[Char], i: Int, s: Array[Char]): Array[Char] = {
    if (!s.isEmpty) {
      str(i) = s.head
      replace(str, i + 1, s.tail)
    }else{
      str
    }
  }

  def rotateFrom(str: Array[Char], char: Char) = {
    val loc = str.indexOf(char)
    val count = 1 + loc + (if (loc >= 4) 1 else 0)
    rotateRight(str, count)
  }

  def unrotate(str: Array[Char], letter: Char): Array[Char] = {


    def doIt(s : Array[Char]) : Array[Char] = {
      val toLeft = rotateLeft(s, 1)
      val res = rotateFrom(toLeft.toSeq.toArray, letter)
      if(str.sameElements(res)){
        toLeft
      }else{
        doIt(toLeft)
      }
    }
    doIt(str.toSeq.toArray)
  }

}
