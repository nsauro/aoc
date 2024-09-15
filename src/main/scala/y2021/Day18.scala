package y2021

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

object Day18 extends App {

  val data = Source.fromResource("2021/18.data").getLines().toSeq.map(toBuffer)

  /*val resBuffer = data.reduce(add)
  val res = resBuffer.mkString("")
  println(res)

  println(y2021.Magnitude.calculate(resBuffer))*/

  println(largestMagnitude)

  def largestMagnitude = {
    val data2 =
      Source.fromResource("2021/18.data").getLines().toSeq.map(toBuffer)

    data2
      .combinations(2)
      .map {
        case first :: second :: tail => {

          val firstSecondAddition = Magnitude.calculate(
            add(ListBuffer.from(first), ListBuffer.from(second))
          )
          val secondFirstAddition = Magnitude.calculate(
            add(ListBuffer.from(second), ListBuffer.from(first))
          )
          Math.max(firstSecondAddition, secondFirstAddition)
        }
      }
      .max
  }

  def toBuffer(str: String): ListBuffer[String] = {
    ListBuffer((str.toCharArray.map(_.toString))*)
  }

  def add(
      first: ListBuffer[String],
      second: ListBuffer[String]
  ): ListBuffer[String] = {
    first.prepend("[")
    first.append(",")
    first.appendAll(second)
    first.append("]")
    reduce(first)
  }

  def reduce(l: ListBuffer[String]): ListBuffer[String] = {

    def repeat(keepGoing: Boolean): ListBuffer[String] = {
      if (keepGoing) {
        val exploded = explode(0, l) // apply explosions to the end
        val splitted = split(0, l)
        repeat(exploded || splitted)
      } else {
        l
      }
    }

    repeat(true)
  }

  def explode(times: Int, l: ListBuffer[String]): Boolean = {

    val res = Exploder.doExplode(l)
    if (res) {
      explode(times + 1, l)
    } else {
      times > 0
    }
  }

  def split(times: Int, l: ListBuffer[String]): Boolean = {
    val res = Splitter.doSplit(l)
    val exploded = Exploder.doExplode(l) // check for explosions

    if (res || exploded) {
      split(times + 1, l)
    } else {
      times > 0
    }
  }

}

object Splitter {

  def doSplit(l: ListBuffer[String]): Boolean = {
    val numberToSplit = l.indexWhere(_.length > 1)
    if (numberToSplit == -1) {
      false
    } else {
      val n = l(numberToSplit).toInt

      val left = n / 2
      val right = n / 2 + (n % 2)

      l(numberToSplit) = "["
      l.insertAll(
        numberToSplit + 1,
        Seq(left.toString, ",", right.toString, "]")
      )
      true
    }
  }

}

object Exploder {

  def doExplode(l: ListBuffer[String]): Boolean = {
    val index = findExplodeIndex(l)
    index match {
      case None => false
      case Some(i) => {
        val firstNumber = l(i + 1).toInt
        val secondNumber = l(i + 3).toInt
        addToLeft(i, firstNumber, l)
        addToRight(i + 3, secondNumber, l)

        l(i) = "0"
        l.remove(i + 1, 4)
        true
      }
    }
  }

  def addToLeft(index: Int, number: Int, l: ListBuffer[String]): Unit = {
    val iToChange = l.lastIndexWhere(isInt, index - 1)
    if (iToChange != -1) {
      l(iToChange) = (l(iToChange).toInt + number).toString
    }
  }

  def addToRight(index: Int, number: Int, l: ListBuffer[String]): Unit = {
    val rightIndex = l.zipWithIndex
      .find { case (v, i) =>
        i > index && isInt(v)
      }
      .map(_._2)

    rightIndex.foreach { iToChange =>
      l(iToChange) = (l(iToChange).toInt + number).toString
    }
  }

  def isInt(s: String): Boolean = Try(s.toInt).isSuccess

  def findExplodeIndex(l: ListBuffer[String]): Option[Int] = {

    case class SplitAcc(depth: Int, index: Option[Int]) {
      def pushLevel(i: Int): SplitAcc = {
        if (index.isDefined) {
          this
        } else {
          val newDepth = depth + 1
          if (newDepth == 5) {
            this.copy(index = Some(i))
          } else {
            this.copy(depth = newDepth)
          }
        }
      }

      def popLevel(): SplitAcc = {
        if (index.isDefined) {
          this
        } else {
          this.copy(depth = depth - 1)
        }
      }
    }

    val res = l.zipWithIndex.foldLeft(SplitAcc(0, None)) { case (sp, (c, i)) =>
      c match {
        case "[" => sp.pushLevel(i)
        case "]" => sp.popLevel()
        case _   => sp
      }
    }
    res.index

  }

}

object Magnitude {

  def calculate(l: ListBuffer[String]): Int = {

    val tree = parseTree(l.iterator)
    println(tree)
    tree.magnitude

  }

  def parseTree(i: Iterator[String]): SNNUmber = {
    val next = i.next()
    next match {
      case "[" => {
        val left = parseTree(i)
        i.next() // kill comma
        val right = parseTree(i)
        i.next() // kill closing
        SNBranch(left, right)
      }
      case digit => {
        SNLeaf(digit.toInt)
      }
    }

  }
}

sealed trait SNNUmber {
  def magnitude: Int
}

case class SNBranch(var left: SNNUmber, var right: SNNUmber) extends SNNUmber {
  override def toString: String = s"[$left,$right]"

  override def magnitude: Int = (3 * left.magnitude) + (2 * right.magnitude)
}

case class SNLeaf(var value: Int) extends SNNUmber {
  override def toString: String = s"$value"

  override def magnitude: Int = value
}
