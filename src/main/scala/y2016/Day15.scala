package y2016

import scala.jdk.CollectionConverters.IteratorHasAsJava

object Day15 extends App {

  val testDiscs = Seq(
    new Disc(5, 4),
    new Disc(2, 1)
  )

  val realDiscs = Seq(
    new Disc(5, 2),
    new Disc(13, 7),
    new Disc(17, 10),
    new Disc(3, 2),
    new Disc(19, 9),
    new Disc(7, 0),
    new Disc(11, 0)
  )

  case class Ball(t: Int, pos: Int) {
    def advance = this.copy(pos = pos + 1)
  }

  class Disc(size: Int, curPos: Int) {
    var curValue = curPos
    def move(): Unit = {
      val n = curValue + 1
      curValue = if (n == size) 0 else n

    }
  }

  val res = findBall(0, Seq(Ball(0, 0)), realDiscs)
  println(res)

  def findBall(time: Int, balls: Seq[Ball], discs: Seq[Disc]): Ball = {
    // when function is invoked, time advances one slot
    val updatedTime = time + 1
    discs.foreach(_.move())
    val updatedBalls = balls.map(_.advance)

    if (updatedBalls.nonEmpty && updatedBalls.head.pos > discs.size) {
      updatedBalls.head
    } else {
      // update discs

      val remainingBalls = updatedBalls.filter { x =>
        // go through discs, find a disc which rejects the ball b/c its location is less < ball loc and its not 0
        discs(x.pos - 1).curValue == 0
      }
      findBall(updatedTime, remainingBalls :+ Ball(updatedTime, 0), discs)

    }

  }

}
