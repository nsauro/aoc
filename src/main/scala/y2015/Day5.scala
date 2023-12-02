package y2015

import scala.io.Source

object Day5 extends App{



  val bad = Set("ab", "cd", "pq", "xy")
  val vowels = Set('a','e','i','o','u')


  val data = Source.fromResource("2015/5.data").getLines()
  println(data.count(isNice2))

  def isNice(str : String) : Boolean = {


    def doTheThing(c : Char, remaining : String, vowelCount: Int, seenDouble: Boolean) : Boolean = {
      val updatedVowelCount = vowelCount + vowels.count(_ == c)
      if(remaining.isEmpty){
        updatedVowelCount >= 3 && seenDouble
      }else{
        val next = remaining.head
        if(bad.contains(s"$c$next")){
          false
        }else{
          val updatedSeenDouble = if(!seenDouble) {
            c == next
          }else{
            seenDouble
          }
          doTheThing(remaining.head, remaining.tail, updatedVowelCount, updatedSeenDouble)
        }
      }

    }

    doTheThing(str.head, str.tail, 0, false)
  }

  def isNice2(str : String) : Boolean = {

    def hasNonOverlappingPairs(pairs : Seq[Seq[(Char, Int)]], charIndices: Map[String , Seq[Set[Int]]]) : Boolean = {
      if(pairs.isEmpty){
        false
      }else{
        val pair = pairs.head
        val key = pair.map(_._1).mkString("")
        val indices = pair.map(_._2).toSet

        charIndices.get(key) match {
          case Some(existingIndices) =>{
            if(existingIndices.exists(x => x.intersect(indices).isEmpty)){
              println(key)
              true
            }else{
              hasNonOverlappingPairs(pairs.tail, charIndices + (key -> (existingIndices :+ indices)))
            }
          }
          case None => hasNonOverlappingPairs(pairs.tail, charIndices + (key -> Seq(indices)))
        }

      }
    }

    val trips = str.sliding(3).collectFirst{case s if s(0) == s(2) => s}
    val isNice = trips.nonEmpty && hasNonOverlappingPairs(str.zipWithIndex.sliding(2).toSeq, Map.empty)
    isNice
  }

}
