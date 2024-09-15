package y2015

import scala.io.Source

object Day7 extends App{

  val data = Source.fromResource("2015/7.data").getLines().toSeq


  val AndParams = raw"""([a-z]+) AND ([a-z]+) -> ([a-z]+)""".r
  val OrParams = raw"""([a-z]+) OR ([a-z]+) -> ([a-z]+)""".r

  val LShiftParams = raw"""([a-z]+) LSHIFT (\d+) -> ([a-z]+)""".r
  val RShiftParams = raw"""([a-z]+) RSHIFT (\d+) -> ([a-z]+)""".r

  val NotParams = raw"""NOT ([a-z]+) -> ([a-z]+)""".r
  val And1Params = raw"""1 AND ([a-z]+) -> ([a-z]+)""".r
  val StartParams = raw"""(\d+) -> ([a-z]+)""".r
  val CopyParams = raw"""([a-z]+) -> ([a-z]+)""".r


  val (initValues, initOperations) = data.foldLeft((Map.empty[String, Int], Map.empty[Seq[String], Seq[Operation]])){
    case ((vs, ops), str) =>str match {
      case AndParams(first, second, res) => (vs, addOperation(ops, Seq(first, second), And(first, second, res)))
      case OrParams(first, second, res) => (vs,  addOperation(ops, Seq(first, second), Or(first, second, res)))
      case LShiftParams(first, amount, res) => (vs, addOperation(ops, Seq(first), LeftShift(first, amount.toInt, res)))
      case RShiftParams(first, amount, res) => (vs, addOperation(ops, Seq(first), RightShift(first, amount.toInt, res)))
      case NotParams(first, res) => (vs, addOperation(ops, Seq(first), Not(first, res)))
      case And1Params(first, res) => (vs, addOperation(ops, Seq(first), And1(first, res)))
      case CopyParams(first, res) => (vs, addOperation(ops, Seq(first), Copy(first, res)))
      case StartParams(amount, res) => (vs + (res -> amount.toInt), ops)
    }
  }

  def addOperation(ops : Map[Seq[String], Seq[Operation]], key : Seq[String], op : Operation) : Map[Seq[String], Seq[Operation]] = {
    val list = ops.getOrElse(key, Seq.empty[Operation])
    ops + (key -> (list :+ op))
  }


  println(initValues)

  //initOperations.foreach(println)

  val result = compute(initOperations, initValues)

  println(result.get("a"))

  def compute(operations : Map[Seq[String], Seq[Operation]], knownRegisters : Map[String, Int]) : Map[String, Int] = {
    if(operations.nonEmpty){
      val knownKeys = knownRegisters.keys.toSet
      val (nextOperations, remainingOperations) = operations.partition(_._1.forall(knownKeys.contains))
      val updatedMap = nextOperations.values.foldLeft(knownRegisters){case (acc, ops) =>
        ops.foldLeft(acc){case (acc2, op) => op.execute(acc2)}
      }
      compute(remainingOperations, updatedMap)
    }else{
      knownRegisters
    }
  }


  trait Operation {

    def execute(registers: Map[String, Int]) : Map[String, Int]

    def resultRegister : String


  }



  case class And(firstRegister : String, secondRegister : String, resultRegister : String) extends Operation {

    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(firstRegister)
      val second = registers(secondRegister)

      registers + (resultRegister -> (first & second))
    }

  }

  case class And1(firstRegister : String, resultRegister : String) extends Operation {

    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(firstRegister)

      registers + (resultRegister -> (first & 1))
    }

  }

  case class Or(firstRegister : String, secondRegister : String, resultRegister : String) extends Operation {

    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(firstRegister)
      val second = registers(secondRegister)

      registers + (resultRegister -> (first | second))
    }

  }

  case class LeftShift(register : String, amount : Int, resultRegister : String) extends Operation {

    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(register)

      registers + (resultRegister -> (first << amount))
    }

  }

  case class RightShift(register : String, amount : Int, resultRegister : String) extends Operation {

    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(register)

      registers + (resultRegister -> (first >>> amount))
    }

  }

  case class Not(register : String,resultRegister : String) extends Operation {


    val mask = 0x0000FFFF
    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(register)

      registers + (resultRegister -> (~first & mask))
    }

  }

  case class Copy(register: String, resultRegister : String) extends Operation {
    def execute(registers: Map[String, Int]) : Map[String, Int] = {

      val first = registers(register)

      registers + (resultRegister -> first)
    }
  }
}
