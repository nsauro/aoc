package y2016

import java.math.BigInteger
import java.security.MessageDigest
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

object Day5 extends App {

  val doorId = "reyedfim"
  // val doorId = "abc"

  val answer = Await.result(findPassword(0, Map.empty), 10.minutes)

  println(answer)

  def findPassword(low: Int, acc: Map[Int, Char]): Future[String] = {
    println(s"finding batch from $low")
    if (acc.size >= 8) {
      Future.successful(acc.toSeq.sortBy(_._1).map(_._2).take(8).mkString(""))
    } else {
      val f = computeBatch(low, low + 1000000)
      f.map { res =>
        res.foldLeft(acc) {
          case (a, (i, c)) => {
            if (!a.contains(i)) {
              a + (i -> c)
            } else {
              a
            }
          }
        }

      }.flatMap { x =>
        findPassword(low + 1000001, x)
      }
    }

  }

  def computeBatch(low: Long, high: Long): Future[Seq[(Int, Char)]] = {
    val f = {
      Future.sequence {
        (low to high).map(computeHash)
      }
    }
    f.map(_.collect {
      case s
          if s.startsWith("00000") && s
            .charAt(5)
            .isDigit && s.charAt(5).toString.toInt < 8 => {
        println(s"got a hit with: $s..returning ${s.charAt(5)}")
        (s.charAt(5).toString.toInt, s.charAt(6))
      }
    })
  }

  def computeHash(l: Long): Future[String] = {
    Future {
      val md = MessageDigest.getInstance("MD5")
      val input = s"$doorId$l"

      // digest() method is called to calculate message digest
      // of an input digest() return array of byte
      val messageDigest = md.digest(input.getBytes)

      // Convert byte array into signum representation
      val no = new BigInteger(1, messageDigest)
      val x = no.toString(16)
      val padding = 32 - x.length
      val hash = x.prependedAll(Seq.fill(padding)("0"))
      if (l == 3231929 || l == 5017308) {
        println(input)
        println(hash)
      }
      hash.mkString("")

    }
  }

}
