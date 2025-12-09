package y2025

object Day06 extends App {

  val data = scala.io.Source.fromResource("2025/6.data")
    .getLines()
    .toArray
    .map(_.split("\\s+").collect{case s if s != "" => s.trim}.toArray)
    .transpose

  val part1 = data.map{ x =>
    val l = x.last
    x.init.map(_.toLong).reduce{(a,b) =>
      if(l == "+") {
        a + b
      } else{
        a * b
      }
    }
  }.sum

  println(part1)

  val data2 = scala.io.Source.fromResource("2025/6.data")
    .getLines()
    .toSeq

  println(doPart2(data2, 0))

  def doPart2(data: Seq[String], acc: Long) : Long = {
    if(data.head == "") {
      acc
    }else{
      val i = data.head.zipWithIndex.find{case(c,i) => data.forall(x => x(i) == ' ')}
      i match{
        case Some((_, index)) => {
          val (toOperate, remaining) = split(data, index)
          val r = evaluate(toOperate)
          doPart2(remaining, acc + r)

        }
        case None => //at end
          val r = evaluate(data)
          acc + r
      }
    }

  }

  def split(data: Seq[String], i : Int) : (Seq[String], Seq[String]) = {
    val r = data.foldLeft((Seq.empty[String], Seq.empty[String]))((acc, row) => {
      val split = row.splitAt(i)
      (acc._1 :+ split._1, acc._2 :+ split._2.tail)
    })
    (r._1, r._2)
  }

  def evaluate(data:Seq[String]): Long = {
    val transposed = data.transpose
    val op = transposed.head.last
    (transposed.head.init +: transposed.tail).map{ x =>
      x.filter(c => c != ' ').mkString("").toLong
    }.reduce{(a,b) =>
      if(op == '*'){
        a * b
      }else{
        a + b
      }
    }
  }





}
