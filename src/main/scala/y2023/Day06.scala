package y2023

object Day06 extends App {

  // val races = Seq((50, 242), (74, 1017), (86, 1691), (85, 1252))
  // val races = Seq((7, 9), (15, 40), (30, 200))
  // val races = Seq((71530, 940200))
  val races = Seq((50748685, 242101716911252L))

  val res = races.map { case (time, dis) =>
    doIt(0, time, dis, 0)
  }.product

  println(res)

  def doIt(
      waitTime: Long,
      raceTime: Long,
      recordDistance: Long,
      wins: Long
  ): Long = {
    if (waitTime == raceTime) {
      wins
    } else {
      val dis = (waitTime) * (raceTime - waitTime)
      val isWin = if (recordDistance < dis) 1 else 0
      doIt(waitTime + 1, raceTime, recordDistance, wins + isWin)
    }
  }
}
