package y2021

import scala.io.Source

object Day24 extends App {


  val allInstructions = Source.fromResource("2021/24.data").getLines().toSeq


  val input = Seq(1L, 4L).iterator


  val magnitude = 10000000000000L
  val maxModel = 99999999999999L

  val generator = new ModelGenerator(11111111111111L, 99999999999999L)

  def findTheThing(modelGenerator: ModelGenerator, acc: Result): Result = {

    if (!modelGenerator.hasNext || acc.model != 0) {
      acc
    } else {
      val next = modelGenerator.next()
      println(next)
      val res = executeProgram(Variables(), allInstructions, Input(next, magnitude))
      println(res)
      val newAcc = if (res.z == 0 && next > acc.model) {
        acc.copy(variables = res, model = next)
      } else {
        acc
      }
      findTheThing(modelGenerator, newAcc)

    }
  }

  val largest = findTheThing(generator, Result(Variables(), 0))
  println(largest)

  class ModelGenerator(start: Long, last: Long) extends Iterator[Long] {


    private var current = last


    override def hasNext: Boolean = current >= start

    private def nextNonZero: Long = {
      val next = current - 1

      def findNextNonZero(v: Long): Long = {
        if (containsZero(v)) {
          findNextNonZero(v - 1)
        } else {
          v
        }
      }

      findNextNonZero(next)

    }

    override def next(): Long = {
      val toReturn = current

      val next = nextNonZero /* current - 1
      val actualNext = if(containsZero(next)){
        nextNonZeroValue(next)
      }else{
        next
      }*/
      current = next
      toReturn
    }

    private def nextNonZeroValue(l: Long): Long = {
      l.toString.replace('0', '9').toLong
    }


    private def containsZero(num: Long): Boolean = {
      if (num == 0) {
        false
      } else {
        if (num % 10 == 0) {
          true
        } else {
          containsZero(num / 10)
        }
      }
    }
  }


  def executeProgram(variables: Variables, instructions: Seq[String], input: Input): Variables = {

    if (instructions.isEmpty) {
      variables
    } else {
      val nextIns = instructions.head
      val (updatedVars, updatedInput) = nextIns match {
        case Instruction.Inp(name) => {
          val (nextValue, updatedInput) = input.next
          (variables.updateValue(name, nextValue), updatedInput)
        }
        case Instruction.Add(dest, value) => (variables.add(dest, value), input)
        case Instruction.Mul(dest, value) => (variables.mul(dest, value), input)
        case Instruction.Div(dest, value) => (variables.div(dest, value), input)
        case Instruction.Mod(dest, value) => (variables.mod(dest, value), input)
        case Instruction.Eql(dest, value) => (variables.eql(dest, value), input)
      }
      executeProgram(updatedVars, instructions.tail, updatedInput)
    }

  }


  object Instruction {

    val Inp = raw"""inp ([wxyz])""".r
    val Add = raw"""add ([wxyz]) ([wxyz\-0-9]+)""".r
    val Mul = raw"""mul ([wxyz]) ([wxyz\-0-9]+)""".r
    val Div = raw"""div ([wxyz]) ([wxyz\-0-9]+)""".r
    val Mod = raw"""mod ([wxyz]) ([wxyz\-0-9]+)""".r
    val Eql = raw"""eql ([wxyz]) ([wxyz\-0-9]+)""".r
  }

  case class Result(variables: Variables, model: Long)

  case class Input(value: Long, magnitude: Long) {

    def next: (Long, Input) = {
      if (magnitude == 0) {
        (value, this)
      } else {
        val nextValue = value / magnitude
        (nextValue, Input(value % magnitude, magnitude / 10))
      }
    }
  }

  case class Variables(w: Long = 0, x: Long = 0, y: Long = 0, z: Long = 0) {

    private val Num = raw"""([\-0-9]+)""".r

    def getValue(str: String): Long = {
      str match {
        case "w" => w
        case "y" => y
        case "x" => x
        case "z" => z
        case Num(n) => n.toLong
      }
    }

    def updateValue(str: String, value: Long): Variables = {
      str match {
        case "w" => this.copy(w = value)
        case "y" => this.copy(y = value)
        case "x" => this.copy(x = value)
        case "z" => this.copy(z = value)
      }
    }

    def add(dest: String, value: String): Variables = {
      val actualValue = getValue(value)
      val destinationValue = getValue(dest)
      updateValue(dest, destinationValue + actualValue)
    }

    def mul(dest: String, value: String): Variables = {
      val actualValue = getValue(value)
      val destinationValue = getValue(dest)
      updateValue(dest, destinationValue * actualValue)
    }

    def div(dest: String, value: String): Variables = {
      val actualValue = getValue(value)
      val destinationValue = getValue(dest)
      updateValue(dest, destinationValue / actualValue)
    }

    def mod(dest: String, value: String): Variables = {
      val actualValue = getValue(value)
      val destinationValue = getValue(dest)
      updateValue(dest, destinationValue % actualValue)
    }

    def eql(dest: String, value: String): Variables = {
      val actualValue = getValue(value)
      val destinationValue = getValue(dest)
      if (destinationValue == actualValue) {
        updateValue(dest, 1)
      } else {
        updateValue(dest, 0)
      }
    }
  }


}
