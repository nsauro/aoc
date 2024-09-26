package y2017

object Day03 extends App:
  val target = 265149

  println(getDistance(3, 1))

  def getDistance(pow: Int, dis: Int): Int = {
    if (pow * pow < target) then getDistance(pow + 2, dis + 1)
    else
      val lowerRight = pow * pow
      val lowerLeft = lowerRight - (pow - 1)
      val upperLeft = lowerLeft - (pow - 1)
      val upperRight = upperLeft - (pow - 1)
      if Seq(lowerRight, lowerLeft, upperRight, upperLeft).contains(target) then
        dis * 2
      else
        val half = Math.floor(pow.doubleValue / 2).toInt
        if target > lowerLeft then dis + Math.abs(target - (lowerLeft + half))
        else if target > upperLeft then
          dis + Math.abs(target - (upperLeft + half))
        else if target > upperRight then
          dis + Math.abs(target - (upperRight + half))
        else dis + Math.abs(target - (upperRight - half))
  }
