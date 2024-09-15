package y2022

import scala.io.Source

object Day05 extends App {

  val data = Source.fromResource("2022/Day05").getLines().toSeq

  val in = data.indexOf("")
  val (_, moves) = data.splitAt(in)

  val stacks = createStacks

  val Instr = raw"""move (\d+) from (\d+) to (\d+)""".r

  /*moves.tail.foreach{
    case Instr(count, fromStack, toStack) => {
      //println(s"$count $fromStack $toStack")
      (0 until count.toInt).foreach{ _ =>
        stacks(toStack).push(stacks(fromStack).pop)
      }
      //stacks.foreach(println)
    }
  }*/

  moves.tail.foreach {
    case Instr(count, fromStack, toStack) => {
      // println(s"$count $fromStack $toStack")
      val moved = (0 until count.toInt).map { _ => stacks(fromStack).pop }
      stacks(toStack).pushAll(moved.reverse)
      stacks.foreach(println)
    }
  }

  println(
    stacks
      .map { case (k, v) => (k.toInt, v) }
      .toSeq
      .sortBy(_._1)
      .map(_._2.head)
      .mkString("")
  )

  def createStacks: Map[String, scala.collection.mutable.Stack[Char]] = {
    val stack1 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('D', 'L', 'V', 'T', 'M', 'H', 'F'))
    val stack2 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('H', 'Q', 'G', 'J', 'C', 'T', 'N', 'P'))
    val stack3 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('R', 'S', 'D', 'M', 'P', 'H'))
    val stack4 =
      scala.collection.mutable.Stack[Char]().pushAll(Seq('L', 'B', 'V', 'F'))
    val stack5 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('N', 'H', 'G', 'L', 'Q'))
    val stack6 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('W', 'B', 'D', 'G', 'R', 'M', 'P'))
    val stack7 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('G', 'M', 'N', 'R', 'C', 'H', 'L', 'Q'))
    val stack8 =
      scala.collection.mutable.Stack[Char]().pushAll(Seq('C', 'L', 'W'))
    val stack9 = scala.collection.mutable
      .Stack[Char]()
      .pushAll(Seq('R', 'D', 'L', 'Q', 'J', 'Z', 'M', 'T'))

    Map(
      "1" -> stack1,
      "2" -> stack2,
      "3" -> stack3,
      "4" -> stack4,
      "5" -> stack5,
      "6" -> stack6,
      "7" -> stack7,
      "8" -> stack8,
      "9" -> stack9
    )

  }

  def testStacks: Map[String, scala.collection.mutable.Stack[Char]] = {
    val stack1 = scala.collection.mutable.Stack[Char]().pushAll(Seq('Z', 'N'))
    val stack2 =
      scala.collection.mutable.Stack[Char]().pushAll(Seq('M', 'C', 'D'))
    val stack3 = scala.collection.mutable.Stack[Char]().pushAll(Seq('P'))

    Map("1" -> stack1, "2" -> stack2, "3" -> stack3)

  }

}
