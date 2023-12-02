package y2015

object Day21 extends App{


  val me = Combatant("player", 0, 0, 100)
  val boss = Combatant("boss", 8, 2, 109)

  val swords = Seq(
    Gear("dagger", 8, 4, 0),
    Gear("shortsword", 10, 5, 0),
    Gear("warhammer", 25, 6, 0),
    Gear("longsword", 40, 7, 0),
    Gear("greataxe", 74, 8, 0),
  )

  val armor = Seq(
    Gear("leather", 13, 0, 1),
    Gear("chainmail", 31, 0, 2),
    Gear("splintmail", 53, 0, 3),
    Gear("bandedmail", 75, 0, 4),
    Gear("platemail", 102, 0, 5)
  )

  val rings = Seq(
    Gear("damge +1", 25, 1, 0),
    Gear("damage +2", 50, 2, 0),
    Gear("damage +3", 100, 3, 0),
    Gear("defense +1", 20, 0, 1),
    Gear("defense +2", 40, 0, 2),
    Gear("defense +3", 80, 0, 3)
  )

  val allRingCombos: Seq[Seq[Gear]] = Seq(
    Seq.empty,
  ) ++ rings.combinations(2).toSeq ++ rings.map(x => Seq(x))


  val swordAndArmorCombos = swords.flatMap{ s =>
    Seq(
      Seq(s)
    ) ++ armor.map(a => Seq(s,a))
  }


  val allTheThings = for{
    sa <- swordAndArmorCombos
    r <- allRingCombos
  } yield {
    sa ++ r
  }

  println(allTheThings.size)

  allTheThings.foreach(println)

  val res = allTheThings.map{ gear =>
    val (totalCost, upgradedPlayer) = gear.foldLeft((0, me)){ case((g, p), gear) =>
      (g + gear.cost, p.copy(armorScore = p.armorScore + gear.armor, damageScore = p.damageScore + gear.damage))
    }
    val winner = fight(upgradedPlayer, boss)
    if(winner.name == "player"){
      totalCost
    }else{
      Int.MaxValue
    }
  }.min

  println(res)


  val res2 = allTheThings.map { gear =>
    val (totalCost, upgradedPlayer) = gear.foldLeft((0, me)) { case ((g, p), gear) =>
      (g + gear.cost, p.copy(armorScore = p.armorScore + gear.armor, damageScore = p.damageScore + gear.damage))
    }
    val winner = fight(upgradedPlayer, boss)
    if (winner.name == "boss") {
      totalCost
    } else {
      Int.MinValue
    }
  }.max

  println(res2)









  def fight(player : Combatant, boss: Combatant) : Combatant = {

    println(s"player: $player -- boss: $boss")
    val playerDamage = Math.max(player.damageScore - boss.armorScore, 1)
    if(boss.hitPointsRemaining - playerDamage > 0){
      val bossDamage = Math.max(boss.damageScore - player.armorScore, 1)
      if(player.hitPointsRemaining - bossDamage > 0){
        fight(player.copy(hitPointsRemaining = player.hitPointsRemaining - bossDamage), boss.copy(hitPointsRemaining = boss.hitPointsRemaining - playerDamage))
      }else{
        boss
      }
    }else{
      player
    }
  }

  case class Combatant(name: String, damageScore : Int, armorScore: Int, hitPointsRemaining : Int)

  case class Gear(name: String, cost: Int, damage : Int, armor : Int)







}
