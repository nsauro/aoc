package y2015

object Day10 extends App {

  val input = "1321131112"

  println(lookAndSay(input.toVector, 50).length)

  def lookAndSay(str: Vector[Char], timesRemaining: Int): Vector[Char] = {
    println(s"str length :${str.length}  -- timesRemaining: $timesRemaining")

    def doTheThing(
        curChar: Char,
        charCount: Int,
        remainingString: Vector[Char],
        result: Vector[Char]
    ): Vector[Char] = {

      if (remainingString.isEmpty) {
        result.appendedAll(Seq(Character.forDigit(charCount, 10), curChar))
      } else {
        val next = remainingString.head
        if (next == curChar) {
          doTheThing(curChar, charCount + 1, remainingString.tail, result)
        } else { // characters changed
          val updatedResult =
            result.appendedAll(Seq(Character.forDigit(charCount, 10), curChar))
          doTheThing(next, 1, remainingString.tail, updatedResult)
        }
      }
    }

    if (timesRemaining == 0) {
      str
    } else {
      val updated = doTheThing(str.head, 1, str.tail, Vector.empty)
      lookAndSay(updated, timesRemaining - 1)
    }
  }

}
