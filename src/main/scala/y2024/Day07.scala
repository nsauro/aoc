package y2024

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day07 extends App:
  val data = Source.fromResource("2024/7.data").getLines().toSeq
  val operatorCache = mutable.LinkedHashMap.empty[Int, Seq[Seq[Char]]]

  val part1 = compute(false)
  println(part1)

  val now = System.currentTimeMillis()
  val part2 = compute(true)
  println(System.currentTimeMillis() - now)
  println(part2)

  def compute(supportConcat: Boolean) = {
    data.foldLeft(0L) { case (acc, row) =>
      val parts = row.split(":")
      val amount = parts(0).toLong
      val nums = parts(1).trim.split(" ").map(_.toLong)
      val operators = getOperators(nums.length - 1, supportConcat)
      val validOperators = operators.exists { ops =>
        val it = ops.iterator
        val n = nums.reduce { case (a, b) =>
          if a < amount then
            it.next() match {
              case '+' => a + b
              case '*' => a * b
              case '|' => (a.toString + b.toString).toLong
              case _ => a
            }
          else
            a
        }
        n == amount
      }
      if validOperators then
        acc + amount
      else
        acc
    }
  }


  def getOperators(amount: Int, supportConcat: Boolean) : Iterator[Seq[Char]] = new Iterator[Seq[Char]]:

    private var internal = ListBuffer.fill(amount)('+')

    override def hasNext: Boolean = internal.nonEmpty

    override def next(): Seq[Char] = {
      val res = internal.toSeq
      genNext(internal.size - 1)
      res
    }

    @tailrec
    private def genNext(i : Int) : Unit = {
      if i < 0 then
        internal = ListBuffer.empty
      else
        internal(i) match{
          case '+' => internal(i) = '*'
          case '*' if supportConcat => internal(i) = '|'
          case '*' if !supportConcat =>
            internal(i) = '+'
            genNext(i - 1)
          case '|' =>
            internal(i) = '+'
            genNext(i - 1)
        }

    }
