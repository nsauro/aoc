package y2016

import scala.io.Source

object Day7 extends App{

  val data = Source.fromResource("2016/7.data").getLines().toSeq.map(_.toCharArray)

/*  println(data.count{x =>
    val res = isTLS(x.toSeq)
    res
  })*/

  println(data.count{ x =>
    val res = isSSL(x.toSeq)
    if (res) {
      println(s"${x.mkString("")} is valid ssl")
      println("--------------")
    }

    res

  })


  def isTLS(s: Seq[Char]) : Boolean = {

    def doIt(remaining: Seq[Char], inBracket:Boolean, isValid:Boolean): Boolean = {

      if(remaining.isEmpty){
        isValid
      }else{

        val boundChar = if(inBracket)']'else'['
        val tBoundLoc = remaining.indexOf(boundChar)
        val boundLoc = if(tBoundLoc < 0){
          remaining.size
        }else{
          tBoundLoc
        }
        val (toCheck, nextCheck) = remaining.splitAt(boundLoc)
        val toCheckisValid = doCheck(toCheck)
        if(toCheckisValid && inBracket){
          false //fail fast
        }else{
          val next = if(nextCheck.isEmpty) nextCheck else nextCheck.tail
          doIt(next, !inBracket, isValid || toCheckisValid)
        }
      }
    }

    doIt(s, false, false)
  }

  def doCheck(s : Seq[Char]) : Boolean = {
    s.sliding(4).exists{ x =>
      val (f, s) = x.splitAt(2)
      f == s.reverse && f.toSet.size > 1 && s.toSet.size > 1
    }
  }

  def isSSL(s : Seq[Char]) : Boolean = {

    def getSequences(remaining: Seq[Char], inBracket: Boolean, aba: Set[Seq[Char]], bab: Set[Seq[Char]]): (Set[Seq[Char]], Set[Seq[Char]]) = {
      if(remaining.isEmpty){
        (aba, bab)
      }else{
        val boundChar = if (inBracket) ']' else '['
        val tBoundLoc = remaining.indexOf(boundChar)
        val boundLoc = if (tBoundLoc < 0) {
          remaining.size
        } else {
          tBoundLoc
        }
        val (toCheck, nextCheck) = remaining.splitAt(boundLoc)
        val (updatedAba, updatedBab) = if(inBracket) (aba, bab ++ getAbas(toCheck)) else (aba ++ getAbas(toCheck), bab)
        val next = if(nextCheck.isEmpty) nextCheck else nextCheck.tail
        getSequences(next, !inBracket, updatedAba, updatedBab)
      }
    }
    val (abas, babs) = getSequences(s, false, Set.empty, Set.empty)

    abas.exists { x =>
      babs.contains(Seq(x(1),x(0),x(1)))
    }



  }

  def getAbas(s : Seq[Char]) : Set[Seq[Char]] = {
    s.sliding(3).filter{x =>
      x(0) == x(2) && (x(0) != x(1))
    }.toSet
  }


}
