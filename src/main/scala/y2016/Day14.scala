package y2016

import java.math.BigInteger
import java.security.MessageDigest

object Day14 extends App{


  val md5Hasher = MessageDigest.getInstance("MD5")
  val format = java.util.HexFormat.of()

  val salt = "yjdafjpo"


  case class Candidate(c : Character, queued: Int, maxIndex: Int, md5: String)

  println(doTheThing(Seq.empty, 0, Set.empty))


  def doTheThing(candidates: Seq[Candidate], currentIndex: Int, keyIndices: Set[Int]) : Int = {
    val remainingCandidates = candidates.filter(_.maxIndex >= currentIndex)
    val md5 = getStretchedMd5(s"$salt$currentIndex", 2017)
    val containsTriples = containsAnyConsecutive(md5)
    /*containsTriples.foreach{ x =>
      println(s"$currentIndex contains triples $md5 -- $x")
    }*/

    val (hits, noHits) = remainingCandidates.partition(x => contains5(md5, x.c))


    val newMatches = keyIndices ++ hits.map(_.queued)
    if(newMatches.size != keyIndices.size){
      println(s"$currentIndex -- ${newMatches.size} -- $md5")
      println(s"hits:")
      hits.foreach(println)
    }
    if(newMatches.size >= 64){
      val sorted = newMatches.toArray.sorted
      sorted.foreach(println)
      sorted(63)
    }else{

      doTheThing(
        containsTriples.fold(noHits)(noHits :+ Candidate(_, currentIndex, currentIndex + 1000, md5)),
        currentIndex + 1,
        newMatches
      )
    }
  }

  def getMd5(s : String, repeat: Int = 1) : String = {

    if(repeat == 0){
      s
    }else{
      md5Hasher.update(s.toLowerCase.getBytes("UTF-8"))
      val digest = md5Hasher.digest
      md5Hasher.reset()
      val bigInt = new BigInteger(1, digest)
      val res = bigInt.toString(16)
      getMd5(res, repeat - 1)
    }
  }

  def getStretchedMd5(s: String, repeat: Int): String = {
    if(repeat == 0){
      s
    }else{

      val res = md5Hasher.digest(s.toLowerCase.getBytes("UTF-8"))
      getStretchedMd5(format.formatHex(res), repeat - 1)
    }
  }

  def hashBytes(bytes : Array[Byte], repeat: Int) : String = {
    if(repeat == 0){

      format.formatHex(bytes)
    }else{
      val res = md5Hasher.digest(bytes)
      hashBytes(res, repeat - 1)
    }
  }


  def contains5(s : String, c : Char) : Boolean = {
    val regex = s".*${Seq.fill(5)(c).mkString("")}.*"
    s.matches(regex)
  }

  def containsAnyConsecutive(md5 : String) : Option[Char] = {

    md5.sliding(3).find(_.toSet.size == 1).map(_.head)

    /*def findIt(s : String, prev: Char, count : Int) : Option[Char] = {
      if(s.isEmpty){
        None
      }else{
        if(s.head != prev && count == 3){
          Some(prev)
        }else if(s.head != prev){
          findIt(s.tail, s.head, 1)
        }else{
          findIt(s.tail, s.head, count + 1)
        }
      }
    }
    findIt(md5.tail, md5.head, 1)*/
  }


}
