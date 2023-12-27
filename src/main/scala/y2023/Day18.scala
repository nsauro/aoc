package y2023


import scala.io.Source

object Day18 extends App{

  val data = Source.fromResource("2023/18.data").getLines().toSeq

  val MoveRight = raw"""R (\d+) \((.+)\)""".r
  val MoveDown = raw"""D (\d+) \((.+)\)""".r
  val MoveLeft = raw"""L (\d+) \((.+)\)""".r
  val MoveUp = raw"""U (\d+) \((.+)\)""".r

  val CorrectMoveRight = raw""".*\(#(.{5})0\)""".r
  val CorrectMoveLeft = raw""".*\(#(.{5})2\)""".r
  val CorrectMoveDown = raw""".*\(#(.{5})1\)""".r
  val CorrectMoveUp = raw""".*\(#(.{5})3\)""".r


  val (rawArea, _, edges) = data.foldLeft((0L, (0,0), 0)){ case  ((acc, p1, edges), ins) =>
    ins match{
      case MoveUp(len, _) => {
        val p2 = p1.copy(_2 = p1._2 + len.toInt)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + len.toInt)
      }
      case MoveRight(len, _) => {
        val p2 = p1.copy(_1 = p1._1 + len.toInt)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + len.toInt)
      }
      case MoveDown(len, _) => {
        val p2 = p1.copy(_2 = p1._2 - len.toInt)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + len.toInt)
      }
      case MoveLeft(len, _) => {
        val p2 = p1.copy(_1 = p1._1 - len.toInt)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + len.toInt)
      }
    }
  }

  println((rawArea.abs / 2) + ((edges / 2) + 1))


  val (rawArea2, _, edges2) = data.foldLeft((0L, (0L, 0L), 0L)) { case ((acc, p1, edges), ins) =>
    ins match {
      case CorrectMoveUp(len) => {
        val l = Integer.parseInt(len, 16)
        val p2 = p1.copy(_2 = p1._2 + l)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + l)
      }
      case CorrectMoveRight(len) => {
        val l = Integer.parseInt(len, 16)
        val p2 = p1.copy(_1 = p1._1 + l)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + l)
      }
      case CorrectMoveDown(len) => {
        val l = Integer.parseInt(len, 16)
        val p2 = p1.copy(_2 = p1._2 - l)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + l)
      }
      case CorrectMoveLeft(len) => {
        val l = Integer.parseInt(len, 16)
        val p2 = p1.copy(_1 = p1._1 - l)
        val newSum = acc + (((p1._1 * p2._2) - (p1._2 * p2._1)))
        (newSum, p2, edges + l)
      }
    }
  }
  println((rawArea2.abs / 2) + ((edges2 / 2) + 1))










}



