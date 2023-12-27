package y2023

import scala.collection.mutable
import scala.io.Source

object Day20 extends App{
  val Low = 0
  val High = 1

  val data = Source.fromResource("2023/20.data").getLines().toSeq

  val rawParams = data.foldLeft(Map.empty[String, Seq[String]]){case (acc, str) =>
    val split = str.split("->")
    acc + (split.head.trim -> split.last.trim.split(",").map(_.trim))
  }

  val modules = rawParams.foldLeft(Map.empty[String, Module]){
    case (acc, (k, v)) =>
      if(k == "broadcaster"){
        acc + ("broadcaster" -> Broadcaster(v))
      }else if(k.startsWith("%")){
        acc + (k.substring(1) -> FlipFlop(k.substring(1), v))
      }else{
        val n = k.substring(1)
        val srcs = rawParams.filter(_._2.contains(n)).keySet.map{ x =>
          if(x  == "broadcaster"){
            x
          }else{
            x.substring(1)
          }

        }
        acc + (n -> Conjunction(n, srcs.toSeq, v))
      }
  }

  modules.foreach(println)



  /*val (presses, lows, highs) = findCycle(mutable.Queue(Signal(Low, "button", "broadcaster")), 1,0,0)
  println(s"presses: $presses -- lows: $lows -- highs: $highs")

  val cycles = 1000 / presses

  println( ((cycles * lows) * (cycles *highs)))*/

  val cycles = Seq("ns", "bh", "dl", "vd").map{ src =>
    println(s"looking for $src")
    modules.values.foreach(_.reset())
    val res = findHighSignalToZh(mutable.Queue(Signal(Low, "button", "broadcaster")), 1, src, Seq.empty)
    println(s"found $src -- $res")
    res.toLong
  }


  def lcm(a: Long, b: Long): Long = (a * b) / gcd(Math.max(a, b), Math.min(a, b))

  //euclid
  def gcd(a: Long, b: Long): Long = {
    if (b == 0) {
      a
    } else {
      val r = a % b
      gcd(b, r)
    }
  }

  cycles.foreach(println)
  println(cycles.reduce(lcm))
  println(cycles.product)
  def findCycle(queue: mutable.Queue[Signal], presses : Int, lows: Int, highs:Int) : (Int, Int, Int) = {
    if(queue.isEmpty){
      if(modules.values.forall(_.inOriginalState) || presses == 1000){
        (presses, lows, highs)
      }else{
        findCycle(queue.addOne(Signal(Low, "button", "broadcaster")), presses + 1, lows, highs)
      }
    }else{
      val newQ = mutable.Queue.empty[Signal]
      val newLows = lows + queue.count(_.value == Low)
      val newHighs = highs + queue.count(_.value == High)
      val updatedQ = queue.foldLeft(newQ){ case (acc, sig) =>
        //println(sig)
        acc.addAll(modules.get(sig.des).map(_.execute(sig)).getOrElse(Seq.empty))
      }
      findCycle(updatedQ, presses, newLows, newHighs)
    }
  }

  def findHighSignalToZh(queue: mutable.Queue[Signal], presses: Int, src:String, signalsToZh: Seq[Signal]): Int = {
    if (queue.isEmpty) {
      if (signalsToZh.nonEmpty && signalsToZh.head.value == High) {
        presses
      } else {
        println(s"presses: $presses -- ${signalsToZh.size} -- ${signalsToZh.exists(_.value == High)}")
        findHighSignalToZh(queue.addOne(Signal(Low, "button", "broadcaster")), presses + 1, src, Seq.empty)
      }
    } else {
      val newQ = mutable.Queue.empty[Signal]
      val (updatedQ, updatedSignals) = queue.foldLeft((newQ, signalsToZh)){ case ((acc, zhAcc), sig) =>
        val updatedZhAcc = if(sig.src == src && sig.des == "zh") zhAcc :+ sig else zhAcc
        (acc.addAll(modules.get(sig.des).map(_.execute(sig)).getOrElse(Seq.empty)), updatedZhAcc)
      }
      findHighSignalToZh(updatedQ, presses, src, updatedSignals)
    }
  }


  trait Module{
    def execute(signal: Signal) : Iterable[Signal]

    def inOriginalState: Boolean

    def reset() : Unit

  }

  case class Broadcaster(destinations: Seq[String]) extends Module {
    val outputs = destinations.map(Signal(0, "broadcaster", _))
    override def execute(signal:Signal): Iterable[Signal] = outputs

    override val inOriginalState: Boolean = true

    override def reset(): Unit = {

    }
  }

  case class FlipFlop(name: String, destination : Seq[String]) extends Module {
    var isOn = false
    val empty = Seq.empty[Signal]

    private val lows = destination.map(Signal(Low, name, _))
    private val highs = destination.map(Signal(High, name, _))

    override def execute(signal: Signal): Iterable[Signal] = {
      if(signal.value != High){
        isOn = !isOn
        if(isOn){
          highs
        }else{
          lows
        }
      }else{
        empty
      }
    }

    override def inOriginalState: Boolean = !isOn

    override def reset(): Unit = isOn = false
  }

  case class Conjunction(name: String, srcs:Seq[String], destination: Seq[String]) extends Module{
    private val memory = mutable.HashMap.empty[String, Int]
    srcs.foreach(x => memory.put(x, Low))

    private val lows = destination.map(Signal(Low, name, _))
    private val highs = destination.map(Signal(High, name, _))
    override def execute(signal: Signal): Iterable[Signal] = {
      memory(signal.src) = signal.value
      if(memory.values.forall(_ == High)){
        lows
      }else{
        highs
      }
    }

    override def inOriginalState: Boolean = memory.values.forall(_ == 0)

    override def reset(): Unit = srcs.foreach(x => memory.put(x, Low))
  }

  case class Signal(value: Int, src: String, des: String) {
    override val toString: String = s"$src -${if(value == High){"high"}else{"low"}}-> $des"
  }
}
