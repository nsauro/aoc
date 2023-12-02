package y2015

import y2021.Day21.Player

object Day22 extends App{

  //854 too low
  //now i got 787

  sealed trait Turn

  case object WizardTurn extends Turn
  case object BossTurn extends Turn


  sealed trait Combatant {
    def name : String
    def hitPointsRemaining : Int
  }

  case class Wizard(name: String, armorScore: Int, manaRemaining: Int, hitPointsRemaining: Int)
  case class Boss(name: String, damageScore: Int, hitPointsRemaining : Int)



  sealed trait Spell {
    val cost : Int
    def cast(player: Wizard, boss: Boss) : (Wizard, Boss, Option[SpellEffect])

    def isCastable(currentMana : Int, activeEffects : Seq[SpellEffect]) : Boolean
  }

  case object Drain extends Spell {
    val cost = 73
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      (player.copy(hitPointsRemaining = player.hitPointsRemaining + 2, manaRemaining = player.manaRemaining - cost), boss.copy(hitPointsRemaining = boss.hitPointsRemaining - 2), None)
    }

    override def isCastable(currentMana: Int, activeEffects: Seq[SpellEffect]): Boolean = currentMana >= cost
  }
  case object MagicMissile extends Spell {
    val cost = 53
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      (player.copy(manaRemaining = player.manaRemaining - cost), boss.copy(hitPointsRemaining = boss.hitPointsRemaining - 4), None)
    }

    override def isCastable(currentMana: Int, activeEffects: Seq[SpellEffect]): Boolean = currentMana >= cost
  }
  case object Shield extends Spell {
    val cost = 113
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      (player.copy(manaRemaining = player.manaRemaining - cost), boss, Some(ShieldEffect(6)))
    }

    override def isCastable(currentMana: Int, activeEffects: Seq[SpellEffect]): Boolean = {
      val shieldEffect = activeEffects.collectFirst{
        case se : ShieldEffect if se.charges > 1 => se
      }
      currentMana >= cost && shieldEffect.isEmpty
    }
  }
  case object Poison extends Spell {
    val cost = 173
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      (player.copy(manaRemaining = player.manaRemaining - cost), boss, Some(PoisonEffect(6)))
    }

    override def isCastable(currentMana: Int, activeEffects: Seq[SpellEffect]): Boolean = {
      val poisonEffect = activeEffects.collectFirst {
        case se: PoisonEffect if se.charges > 1 => se
      }
      currentMana >= cost && poisonEffect.isEmpty
    }
  }
  case object Recharge extends Spell {
    val cost = 229
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      (player.copy(manaRemaining = player.manaRemaining - cost), boss, Some(RechargeEffect(5)))
    }

    override def isCastable(currentMana: Int, activeEffects: Seq[SpellEffect]): Boolean = {
      val chargeEffect = activeEffects.collectFirst {
        case se: RechargeEffect if se.charges > 1 => se
      }
      currentMana >= cost && chargeEffect.isEmpty
    }
  }

  case object NopSpell extends Spell {
    val cost = 0

    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = (player, boss, None)

    override def isCastable(currentMana: Int, activeEffects: Seq[SpellEffect]): Boolean = true
  }


  trait SpellEffect {
    def charges : Int
    def cast(player : Wizard, boss: Boss) : (Wizard, Boss, Option[SpellEffect])
  }

  case class ShieldEffect(charges : Int) extends SpellEffect {
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      val effect = if(charges - 1 == 0){
        Some(ShieldRemoveEffect)
      }else{
        Some(this.copy(charges - 1))
      }
      val armorUpdate = if(charges == 6) 7 else 0
      (player.copy(armorScore = player.armorScore + armorUpdate), boss, effect)
    }
  }

  case object ShieldRemoveEffect extends SpellEffect {
    val charges = 0
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      (player.copy(armorScore = player.armorScore - 7), boss, None)
    }
  }

  case class PoisonEffect(charges: Int) extends SpellEffect {
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      val effect = if (charges - 1 == 0) {
        None
      } else {
        Some(this.copy(charges - 1))
      }
      (player, boss.copy(hitPointsRemaining = boss.hitPointsRemaining - 3), effect)
    }
  }

  case class RechargeEffect(charges: Int) extends SpellEffect {
    override def cast(player: Wizard, boss: Boss): (Wizard, Boss, Option[SpellEffect]) = {
      val effect = if (charges - 1 == 0) { //5i, 4i, 3i, 2i, 1i
        None
      } else {
        Some(this.copy(charges - 1))
      }
      (player.copy(manaRemaining = player.manaRemaining + 101), boss, effect)
    }
  }


  val allSpells = Seq(Drain, MagicMissile, Shield, Poison, Recharge)

  val wizard = Wizard("wiz", 0, 500, 50)
  val boss = Boss("boss", 8, 55)



  var bestScore = Int.MaxValue
  allSpells.map(sp => fight(wizard, boss, sp, Seq.empty, 0, WizardTurn, true)).min
  println(bestScore)



  def fight(wizard: Wizard, boss: Boss, spell: Spell, activeEffects: Seq[SpellEffect], manaUsed: Int, turn : Turn, hardMode: Boolean) : Unit = {

    val w2 = if(hardMode && turn == WizardTurn){
      wizard.copy(hitPointsRemaining = wizard.hitPointsRemaining - 1)
    }else {
      wizard
    }

    if(w2.hitPointsRemaining <= 0){
      println("wiz bled out")
    }else{
      //run effects
      val (updatedWizard, updatedBoss, remainingEffects) = activeEffects.foldLeft((w2, boss, Seq.empty[SpellEffect])){
        case ((w, b, acc), e) => {
          val (ew, eb, re) = e.cast(w, b)
          (ew, eb, acc ++ re)
        }
      }
      //if anyone is dead, stop
      if(updatedBoss.hitPointsRemaining <= 0){
        if (manaUsed < bestScore) {
          bestScore = manaUsed
        }
        println("effect killed the boss")
      }else if(updatedWizard.hitPointsRemaining <= 0) {
        println("wiz is dead")
      }else{
        turn match {
          case WizardTurn => {
            //only cast if this keeps us below the best score
            if(manaUsed + spell.cost <= bestScore){

              val (ew, eb, re) = spell.cast(updatedWizard, updatedBoss)
              val updatedMana = manaUsed + spell.cost
              if(eb.hitPointsRemaining <= 0){ //boss is dead
                println(s"boss is dead: $updatedMana")
                if(updatedMana < bestScore){
                  bestScore = updatedMana
                }
              }else{
                fight(ew, eb, spell, remainingEffects ++ re, updatedMana, BossTurn, hardMode)
              }
            }else{
              //no point in continuing, any additional
            }
          }
          case BossTurn => {
            val dmg = Math.max(updatedBoss.damageScore - updatedWizard.armorScore, 1)
            val ew = updatedWizard.copy(hitPointsRemaining = updatedWizard.hitPointsRemaining - dmg)
            if(ew.hitPointsRemaining <= 0) { //player is dead
              println("wiz is dead")
            }else{
              //both boss and wizard survived, run the next round with all affordable spells
              val affordableSpells = allSpells.filter(_.isCastable(ew.manaRemaining, remainingEffects))
              (affordableSpells, remainingEffects) match {
                case (Nil, Nil) => println("wiz is toast")
                case (Nil, ef) => fight(ew, updatedBoss, NopSpell, ef, manaUsed, WizardTurn, hardMode) //no spells..but active effects..maybe he can still win
                case (spells, ef) => {
                  spells.foreach{ s =>
                    fight(ew, updatedBoss, s, ef, manaUsed, WizardTurn, hardMode)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
