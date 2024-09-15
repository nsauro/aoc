package y2015

object Day14 extends App {

  case class Reindeer(
      kmS: Int,
      flight: Int,
      rest: Int,
      remaining: Int,
      state: String = "flying",
      points: Int = 0,
      totalDistance: Int = 0
  )

  val reindeers = Seq(
    Reindeer(8, 8, 53, 8),
    Reindeer(13, 4, 49, 4),
    Reindeer(20, 7, 132, 7),
    Reindeer(12, 4, 43, 4),
    Reindeer(9, 5, 38, 5),
    Reindeer(10, 4, 37, 4),
    Reindeer(3, 37, 76, 37),
    Reindeer(9, 12, 97, 12),
    Reindeer(37, 1, 36, 1)
  )

  val res = race(2503, reindeers)
  println(res.max)

  val testReindeers = Seq(
    Reindeer(14, 10, 127, 10),
    Reindeer(16, 11, 162, 11)
  )

  println(race2(2503, reindeers))

  def race(totalSeconds: Int, reindeer: Seq[Reindeer]): Seq[Int] = {
    reindeer.map { r =>
      val interval = r.rest + r.flight
      val totalIntervals = totalSeconds / interval
      val traveled = totalIntervals * r.flight * r.kmS
      val leftOver = totalSeconds % interval
      val remaining = Math.min(leftOver, r.flight)
      traveled + (remaining * r.kmS)
    }
  }

  def race2(secondsRemaining: Int, reindeer: Seq[Reindeer]): Int = {

    if (secondsRemaining < 0) {
      reindeer.map(_.points).max
    } else {
      val updated = reindeer.map { x =>
        val newDistance = if (x.state == "flying") {
          x.kmS
        } else {
          0
        }
        val (updatedRemaining, updatedState) =
          if (x.remaining == 1 && x.state == "flying") {
            (x.rest, "resting")
          } else if (x.remaining == 1 && x.state == "resting") {
            (x.flight, "flying")
          } else {
            (x.remaining - 1, x.state)
          }

        x.copy(
          remaining = updatedRemaining,
          state = updatedState,
          totalDistance = x.totalDistance + newDistance
        )

      }

      val furthestDistance = updated.sortBy(-_.totalDistance).head.totalDistance

      val updatedPoints = updated.map { x =>
        if (x.totalDistance == furthestDistance) {
          x.copy(points = x.points + 1)
        } else {
          x
        }
      }

      race2(secondsRemaining - 1, updatedPoints)

    }

  }

}
