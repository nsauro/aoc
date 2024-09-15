package y2021

object Day17 extends App {

  // input = x=25..67, y=-260..-200

  case class TargetArea(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

  val myTargetArea = TargetArea(25, 67, -200, -260)
  // val myTargetArea = TargetArea(20, 30, -5, -10)

  val highPoints = for {
    x <- 0 to myTargetArea.xMax
    y <- myTargetArea.yMin to myTargetArea.yMax * -1
  } yield {
    computeApex(0, 0, x, y, 0)
  }

  println(highPoints.max)

  val validCoords = (for {
    x <- 0 to myTargetArea.xMax
    y <- myTargetArea.yMax to myTargetArea.yMax * -1
  } yield {
    if (landsIt(0, 0, x, y, 0)) {
      Some((x, y))
    } else {
      None
    }
  }).flatten

  // validCoords.foreach(println)
  val pathCount = validCoords.size
  println(pathCount)

  def computeApex(
      x: Int,
      y: Int,
      xVelocity: Int,
      yVelocity: Int,
      highestY: Int
  ): Int = {
    // println(s"$x - $y")
    if (isInTarget(x, y)) {
      highestY
    } else if (isPastTarget(x, y)) { // bad coords
      0
    } else {
      val newX = x + xVelocity
      val newY = y + yVelocity
      val newHighestY = Math.max(newY, highestY)
      computeApex(
        newX,
        newY,
        decreaseX(xVelocity),
        decreaseY(yVelocity),
        newHighestY
      )
    }
  }

  def landsIt(
      x: Int,
      y: Int,
      xVelocity: Int,
      yVelocity: Int,
      highestY: Int
  ): Boolean = {
    // println(s"$x - $y")
    if (isInTarget(x, y)) {
      true
    } else if (isPastTarget(x, y)) { // bad coords
      false
    } else {
      val newX = x + xVelocity
      val newY = y + yVelocity
      val newHighestY = Math.max(newY, highestY)
      landsIt(
        newX,
        newY,
        decreaseX(xVelocity),
        decreaseY(yVelocity),
        newHighestY
      )
    }
  }

  def decreaseX(x: Int): Int = {
    if (x > 0) {
      x - 1
    } else if (x < 0) {
      x + 1
    } else {
      x
    }
  }

  def decreaseY(y: Int): Int = {
    y - 1
  }

  def isInTarget(x: Int, y: Int): Boolean = {
    x >= myTargetArea.xMin && x <= myTargetArea.xMax && y <= myTargetArea.yMin && y >= myTargetArea.yMax
  }

  def isPastTarget(x: Int, y: Int): Boolean = {
    x > myTargetArea.xMax || y < myTargetArea.yMax
  }

}
