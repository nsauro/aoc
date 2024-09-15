package y2015

import scala.io.Source

object Day24 extends App {

  val data = Source.fromResource("2015/24.data").getLines().toSeq.map(_.toLong)

  val weight = data.sum / 4
  println(s"target weight: $weight")

  val bags = LazyList
    .from((4 to data.size - 3))
    .flatMap(x =>
      LazyList.from(data.combinations(x)).collect {
        case x if x.sum == weight => (x.size, x.product)
      }
    )

  println(bags.min)

}
