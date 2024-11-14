package y2017

object Day10 extends App:

  val hash = (0L to 255L).toArray


  val lengths = Seq(31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33)
  //val lengths = Seq(3,4,1,5)

  val (updatedH, pos, skipSize) = doTheHash(hash, lengths, 0, 0)
  println(updatedH.head * updatedH(1))

  def doTheHash(h: Array[Long], l: Seq[Int], pos: Int, skipSize: Int) : (Array[Long], Int, Int) = {
    if l.isEmpty then
      (h, pos, skipSize)
    else
      val indices = (pos until pos + l.head)
      val elements = indices.map{ i =>
        if i < h.length then
          h(i)
        else
          h(i % h.length)
      }.reverse
      (indices zip elements).foreach{
        case(i, e) =>
          if i < h.length then
            h(i) = e
          else
            h(i % h.length) = e
      }
      doTheHash(h, l.tail, pos + l.head + skipSize, skipSize + 1)
  }

  val res2 = doPart2()
  println(res2)

  def doPart2() : String = {
    val h = (0L to 255L).toArray
    val rawLengths = "31,2,85,1,80,109,35,63,98,255,0,13,105,254,128,33"
    val asciiCodes = rawLengths.map(_.toInt)  ++ Seq(17, 31, 73, 47, 23)

    def doIt(timesLeft: Int, pos: Int, skipSize: Int) : Unit = {
      if timesLeft != 0 then
        val (updatedH, newPos, newSkipSize) = doTheHash(h, asciiCodes, pos, skipSize)
        doIt(timesLeft - 1, newPos, newSkipSize)
    }

    doIt(64, 0, 0)
    h.grouped(16).map{x =>
      val str = x.reduce(_ ^ _).toHexString
      if(str.length < 2)
        '0' + str
      else
        str
      }.mkString("")
  }