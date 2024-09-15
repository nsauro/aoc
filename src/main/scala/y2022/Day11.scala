package y2022

import scala.collection.mutable.{ListBuffer, Map => MMap}

object Day11 extends App {

  val testData = MMap(
    0 -> Monkey(0, ListBuffer(79, 98), _ * 19, 23, 2, 3, 0),
    1 -> Monkey(1, ListBuffer(54, 65, 75, 74), _ + 6, 19, 2, 0, 0),
    2 -> Monkey(2, ListBuffer(79, 60, 97), x => x * x, 13, 1, 3, 0),
    3 -> Monkey(3, ListBuffer(74), _ + 3, 17, 0, 1, 0)
  )

  val myData = MMap(
    0 -> Monkey(0, ListBuffer(59, 65, 86, 56, 74, 57, 56), _ * 17, 3, 3, 6, 0),
    1 -> Monkey(1, ListBuffer(63, 83, 50, 63, 56), _ + 2, 13, 3, 0, 0),
    2 -> Monkey(2, ListBuffer(93, 79, 74, 55), _ + 1, 2, 0, 1, 0),
    3 -> Monkey(
      3,
      ListBuffer(86, 61, 67, 88, 94, 69, 56, 91),
      _ + 7,
      11,
      6,
      7,
      0
    ),
    4 -> Monkey(4, ListBuffer(76, 50, 51), x => x * x, 19, 2, 5, 0),
    5 -> Monkey(5, ListBuffer(77, 76), _ + 8, 17, 2, 1, 0),
    6 -> Monkey(6, ListBuffer(74), _ * 2, 5, 4, 7, 0),
    7 -> Monkey(7, ListBuffer(86, 85, 52, 86, 91, 95), _ + 6, 7, 4, 5, 0)
  )

  val megaPrime = myData.values.map(_.test).product
  val updatedMonkeys = compute(1, 10000, myData)

  val business = updatedMonkeys
    .sortBy(x => -x.itemsHandled)
    .take(2)
    .map(_.itemsHandled)
    .reduce(_ * _)
  println(business)

  def compute(
      curRound: Int,
      maxRound: Int,
      monkeys: MMap[Int, Monkey]
  ): Seq[Monkey] = {
    if (curRound > maxRound) {
      monkeys.values.toSeq
    } else {
      monkeys.iterator.toSeq.sortBy(_._1).foreach { case (i, monkey) =>
        monkey.items.foreach { item =>
          val worry = monkey.worryUpdate(item) % megaPrime
          if (worry % monkey.test == 0) {
            monkeys(monkey.trueMonkey).items.addOne(worry)
          } else {
            monkeys(monkey.falseMonkey).items.addOne(worry)
          }
        }
        monkey.itemsHandled = monkey.itemsHandled + monkey.items.size
        monkey.items.remove(0, monkey.items.size)
      }

      compute(curRound + 1, maxRound, monkeys)

    }

  }

  // 1183848012
  // 14068749900
  case class Monkey(
      id: Int,
      items: ListBuffer[Long],
      worryUpdate: Long => Long,
      test: Int,
      trueMonkey: Int,
      falseMonkey: Int,
      var itemsHandled: Long
  )
}
