package y2015

object Day15 extends App {

  case class Ingredient(
      capacity: Int,
      durability: Int,
      flavor: Int,
      texture: Int,
      calories: Int
  )

  val frosting = Ingredient(4, -2, 0, 0, 5)
  val candy = Ingredient(0, 5, -1, 0, 8)
  val butterscotch = Ingredient(-1, 0, 5, 0, 6)
  val sugar = Ingredient(0, 0, -2, 2, 1)

  val allFrosting = (0 to 99).map(_ => frosting)
  val allCandy = (0 to 99).map(_ => candy)
  val allButterscotch = (0 to 99).map(_ => butterscotch)
  val allSugar = (0 to 99).map(_ => sugar)

  val all = allFrosting ++ allCandy ++ allButterscotch ++ allSugar

  val allCombos = all.combinations(100)

  val best = allCombos.foldLeft(0L) { case (acc, s) =>
    val (capacity, durability, flavor, texture, calories) =
      s.foldLeft((0, 0, 0, 0, 0)) { case (acc, x) =>
        (
          acc._1 + x.capacity,
          acc._2 + x.durability,
          acc._3 + x.flavor,
          acc._4 + x.texture,
          acc._5 + x.calories
        )
      }

    val total = Math.max(capacity, 0) * Math.max(durability, 0) * Math.max(
      flavor,
      0
    ) * Math.max(texture, 0)
    if ((total > acc) && calories <= 500) {
      total
    } else {
      acc
    }
  }

  println(best)

}
