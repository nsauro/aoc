package y2016

object Day16 extends App {

  val length = 35651584

  val input = "10001001100000001"

  println(fromBooleans(checkSum(expand(toBooleans(input)).take(length))))

  def expand(s: Seq[Boolean]): Seq[Boolean] = {
    if (s.size >= length) {
      s
    } else {
      val copy = Seq.from(s).reverse.map(!_)
      expand(s ++ Seq(false) ++ copy)
    }

  }

  def checkSum(s: Seq[Boolean]): Seq[Boolean] = {
    val res = s
      .grouped(2)
      .map { x =>
        !(x.head ^ x.last)
      }
      .toSeq
    if (res.size % 2 == 0) {
      checkSum(res)
    } else {
      res
    }
  }

  def toBooleans(s: String): Seq[Boolean] = {
    s.map(_ == '1')
  }

  def fromBooleans(s: Seq[Boolean]) = {
    s.map(x => if (x) '1' else '0').mkString
  }

}
