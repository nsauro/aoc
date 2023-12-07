package y2023

import scala.io.Source

object Day07 extends App{

  val `5Kind` = 6
  val `4Kind` = 5
  val FullHouse = 4
  val `3Kind` = 3
  val `2Pair` = 2
  val `1Pair` = 1
  val NoMatch = 0

  val cardOrder = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')

  val cardOrderWild = Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')

  val data = Source.fromResource("2023/7.data").getLines().toSeq

  val parsed = data.map{ x =>
    val parts = x.split(" ")
    (parts.head, parts.last.toInt)
  }

  val res = parsed.sortWith{ case (a, b) =>

    val aHand = determineHand(a._1)
    val bHand = determineHand(b._1)
    if(aHand != bHand){
      aHand < bHand
    }else{
      orderByCards(a._1, b._1, cardOrder)
    }
  }.zipWithIndex.map{case ((_, b), r) => b * (r + 1)}.sum

  println(res)


  val res2 = parsed.sortWith { case (a, b) =>

    val aHand = determineWildHand(a._1)
    val bHand = determineWildHand(b._1)
    if (aHand != bHand) {
      aHand < bHand
    } else {
      orderByCards(a._1, b._1, cardOrderWild)
    }
  }.zipWithIndex.map { case ((_, b), r) => b * (r + 1) }.sum

  println(res2)

  def determineHand(s : String) = {
    val set = s.toSet
    if(set.size == 5){ //no matches
      NoMatch
    }else if(set.size == 4) { //one pair
      //j=0, j=1,j=2?
      `1Pair`
    } else if(set.size == 3) { //two pair,three of a kind
      //j=1,j=2,j=3
      if(set.exists(x => s.count(_ == x) == 3)){ //three of a kind
        `3Kind`
      }else{ //two pair
        `2Pair`
      }
    } else if(set.size == 2) { // //four of a kind, full house
        //j=1, j=2, j=3, j=4
        if(set.exists(x => s.count(_ == x) == 4)){ //four ofa kind
          `4Kind`
        }else{ //fullhouse
          FullHouse
        }
    } else { //5 of a kind
      `5Kind`
    }
  }

  def determineWildHand(s: String) = {
    val set = s.toSet
    val jCount = s.count(_ == 'J')
    if (set.size == 5) { //no matches
      jCount //already correct
    } else if (set.size == 4) { //one pair
      promotePair(jCount)
    } else if (set.size == 3) { //two pair,three of a kind
      //j=1,j=2,j=3
      if (set.exists(x => s.count(_ == x) == 3)) { //three of a kind
        promote3Kind(jCount)
      } else { //two pair
        promote2Pair(jCount)
      }
    } else if (set.size == 2) { // //four of a kind, full house
      //j=1, j=2, j=3, j=4
      if (set.exists(x => s.count(_ == x) == 4)) { //four ofa kind
        promote4Kind(jCount)
      } else { //fullhouse
        promoteFullHouse(jCount)
      }
    } else { //5 of a kind
      `5Kind`
    }
  }

  def promotePair(jCount : Int) = {
    if(jCount == 0){
      `1Pair`
    }else{
      `3Kind`
    }
  }

  def promote2Pair(jCount: Int) = {
    if(jCount == 0){
      `2Pair`
    }else if(jCount == 1){
      FullHouse
    }else {
      `4Kind`
    }
  }
  def promote3Kind(jcount: Int) = {
    if(jcount == 0){
      `3Kind`
    }else if(jcount == 1){
      `4Kind`
    }else if (jcount == 2){
      `5Kind`
    }else {
      `4Kind`
    }
  }

  def promoteFullHouse(jCount: Int) = {
    if(jCount == 0){
      FullHouse
    }else if(jCount == 1){
      `4Kind`
    }else{
      `5Kind`
    }
  }

  def promote4Kind(jCount: Int) = {
    if(jCount == 0){
      `4Kind`
    }else{
      `5Kind`
    }
  }

  def orderByCards(a : Seq[Char], b: Seq[Char], order:Seq[Char]) : Boolean = {
    if(a.isEmpty){
      true
    }else if(a.head == b.head){
      orderByCards(a.tail, b.tail, order)
    }else{
      order.indexOf(b.head) < order.indexOf(a.head)
    }
  }
}


