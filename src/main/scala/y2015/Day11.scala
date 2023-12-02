package y2015

object Day11 extends App{


  val charValues = Map(
    'a' -> 1,
    'b' -> 2,
    'c' -> 3,
    'd' -> 4,
    'e' -> 5,
    'f' -> 6,
    'g' -> 7,
    'h' -> 8,
    'j' -> 9,
    'k' -> 10,
    'm' -> 11,
    'n' -> 12,
    'p' -> 13,
    'q' -> 14,
    'r' -> 15,
    's' -> 16,
    't' -> 17,
    'u' -> 18,
    'v' -> 19,
    'w' -> 20,
    'x' -> 21,
    'y' -> 22,
    'z' -> 23,
  )

  val intValues: Map[Int, Char] = charValues.map(x => (x._2, x._1))

  val allowedValues = charValues.values.toVector.sorted


  val input = "cqjxxyzz"

  val encoded = input.reverse.map(charValues(_))


  val res = findNextPassword(increment(encoded))
  println(decode(res))



  def decode(i : Seq[Int]) : String = {
    i.reverse.map(intValues(_)).mkString("")
  }



  def getNextValue(i : Int) : Int = {
    val next = allowedValues.indexOf(i) + 1
    if(next == allowedValues.size){
      allowedValues(0)
    }else{
      allowedValues(next)
    }
  }

  def increment(pwd : Seq[Int]) : Seq[Int] = {

    def doTheThing(cur : Seq[Int], updated: Seq[Int]) : Seq[Int] = {
      if(cur.isEmpty){
        updated
      }else{
        val next = getNextValue(cur.head)
        if(next == 1){
          doTheThing(cur.tail, updated :+ next)
        }else{
          (updated :+ next) ++ cur.tail
        }
      }
    }

    doTheThing(pwd, Seq.empty)
  }

  def isValidPassword(pwd : Seq[Int]) : Boolean = {
    val hasStraight = pwd.sliding(3).exists(x => (x.head - x(1) == 1) && (x(1) - x.last == 1))
    val hasPairs = pwd.sliding(2).toSeq.distinct.map(_.toSet).filter(_.size == 1).flatten.size > 1

    hasPairs && hasStraight

  }


  def findNextPassword(pwd : Seq[Int]) : Seq[Int] = {
    if(isValidPassword(pwd)){
      pwd
    }else{
      findNextPassword(increment(pwd))
    }
  }


}
