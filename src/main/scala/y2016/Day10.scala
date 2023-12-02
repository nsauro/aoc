package y2016

import scala.io.Source

object Day10 extends App{


  val data = Source.fromResource("2016/10.data").getLines().toSeq

  val InitValue = raw"""value (\d+) goes to bot (\d+)""".r
  val GiveInst = raw"""bot (\d+) gives low to (.+) (\d+) and high to (.+) (\d+)""".r


  val bots = parseInstructions(data, Map.empty)
  //bots.foreach(println)

  println(executeUntil(bots, Map.empty, 17, 61))

  println(executeUntilDone(bots, Map.empty).filter(x => Set(0,1,2).contains(x._1.id)).map(_._2.chips.head).product)



  def parseInstructions(ins : Seq[String], bots: Map[BotId, Bot]) :  Map[BotId, Bot] = {
    if(ins.isEmpty){
      bots
    }else {
      ins.head match {
        case InitValue(amt, botId) => {
          val updatedBot = bots.get(BotId(botId.toInt)).fold(Bot(BotId(botId.toInt), Seq(amt.toInt), BotId(0), BotId(0)))(x => x.copy(chips = x.chips :+ amt.toInt))
          parseInstructions(ins.tail, bots + (updatedBot.id -> updatedBot))
        }
        case GiveInst(botId, lowDest, lowAmt, highDest, highAmt) => {
          val updatedBot = bots.get(BotId(botId.toInt)).fold(Bot(BotId(botId.toInt), Seq.empty, Id(lowDest, lowAmt.toInt), Id(highDest, highAmt.toInt)))(x => x.copy(lowTo = Id(lowDest, lowAmt.toInt), highTo = Id(highDest, highAmt.toInt)))
          parseInstructions(ins.tail, bots + (updatedBot.id -> updatedBot))
        }
      }
    }
  }

  def executeUntil(bots: Map[BotId, Bot], outputs: Map[OutputId, Output], low: Int, high: Int) : Int = {
    val matchingBot = bots.values.collectFirst{
      case b : Bot if b.chips.toSet == Set(low, high) =>  {
        outputs.foreach(println)
        b
      }
    }

    matchingBot match {
      case None => {
        val nextBot = bots.values.collectFirst{
          case b : Bot if b.chips.size == 2 => b
        }
        nextBot match {
          case None => ???
          case Some(b) => {

            val (lowUpdatedOutputs, lowUpdatedBots) = update(b.lowTo, outputs, bots, b.chips.min)
            val (highUpdatedOutputs, highUpdatedBots) = update(b.highTo, lowUpdatedOutputs, lowUpdatedBots, b.chips.max)
            val updated =  highUpdatedBots + (b.id -> b.copy(chips = Seq.empty))
            executeUntil(updated, highUpdatedOutputs, low, high)
          }
        }
      }
      case Some(b) => b.id.id
    }
  }

  def executeUntilDone(bots: Map[BotId, Bot], outputs: Map[OutputId, Output]): Map[OutputId, Output] = {
    val nextBot = bots.values.collectFirst {
      case b: Bot if b.chips.size == 2 => b
    }
    nextBot match {
      case None => outputs
      case Some(b) => {

        val (lowUpdatedOutputs, lowUpdatedBots) = update(b.lowTo, outputs, bots, b.chips.min)
        val (highUpdatedOutputs, highUpdatedBots) = update(b.highTo, lowUpdatedOutputs, lowUpdatedBots, b.chips.max)
        val updated = highUpdatedBots + (b.id -> b.copy(chips = Seq.empty))
        executeUntilDone(updated, highUpdatedOutputs)
      }
    }
  }


  def update(id: Id, outputs: Map[OutputId, Output], bots: Map[BotId, Bot], chip : Int) : (Map[OutputId, Output], Map[BotId, Bot]) = {
    id match {
      case botId: BotId => (outputs, updateBots(botId, bots, chip))
      case outputId: OutputId => (updateOutputs(outputId, outputs, chip), bots)
    }
  }

  def updateOutputs(outputId : OutputId, outputs: Map[OutputId, Output], chip: Int) = {
    outputs.get(outputId) match {
      case None => {
        outputs + (outputId -> Output(outputId, Seq(chip)))
      }
      case Some(x) => outputs + (outputId -> x.addChip(chip).asInstanceOf[Output])
    }
  }

  def updateBots(botId: BotId, bots: Map[BotId, Bot], chip : Int) = {
    bots + (botId -> bots(botId).addChip(chip).asInstanceOf[Bot])
  }



  sealed trait Id extends Any with Product with Serializable {
    def id : Int
  }

  case class BotId(id : Int) extends Id
  case class OutputId(id: Int) extends Id



  object Id {
    def apply(str: String, id : Int) = {
      if(str == "output"){
        OutputId(id)
      }else{
        BotId(id)
      }
    }
  }

  trait Container[ID <: Id] {
    def id : ID
    def chips : Seq[Int]
    def addChip(c : Int) : Container[ID]
  }

  case class Bot(id : BotId, chips: Seq[Int], lowTo: Id, highTo: Id) extends Container[BotId] {
    override def addChip(c: Int): Container[BotId] = this.copy(chips = chips :+ c)
  }

  case class Output(id : OutputId, chips: Seq[Int]) extends Container[OutputId] {
    override def addChip(c: Int): Container[OutputId] = this.copy(chips = chips :+ c)
  }


}
