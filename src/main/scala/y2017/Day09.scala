package y2017

import scala.io.Source

object Day09 extends App:

  val data = Source.fromResource("2017/9.data").toSeq
  // val data = "{{<a!>},{<a!>},{<a!>},{<ab>}}".toArray

  println(doTheThing(data, 0, 0, false, false))

  println(countGarbage(data, 0, false, false))

  def doTheThing(
      s: Seq[Char],
      depth: Int,
      acc: Int,
      inGarbage: Boolean,
      ignoreNext: Boolean
  ): Int = {
    if s.isEmpty then acc
    else if !inGarbage then
      s.head match
        case '{' => doTheThing(s.tail, depth + 1, acc, false, false)
        case '}' => doTheThing(s.tail, depth - 1, acc + depth, false, false)
        case ',' => doTheThing(s.tail, depth, acc, false, false)
        case '<' => doTheThing(s.tail, depth, acc, true, false)
        case o =>
          println(s"bad: $o")
          1
    else if ignoreNext then doTheThing(s.tail, depth, acc, inGarbage, false)
    else if inGarbage then
      s.head match
        case '>' => doTheThing(s.tail, depth, acc, false, false)
        case '!' => doTheThing(s.tail, depth, acc, true, true)
        case _   => doTheThing(s.tail, depth, acc, true, false)
    else
      println("bad!")
      1
  }

  def countGarbage(
      s: Seq[Char],
      acc: Int,
      inGarbage: Boolean,
      ignoreNext: Boolean
  ): Int = {
    if s.isEmpty then acc
    else if !inGarbage then
      s.head match
        case '<' => countGarbage(s.tail, acc, true, false)
        case o   => countGarbage(s.tail, acc, false, false)
    else if ignoreNext then countGarbage(s.tail, acc, inGarbage, false)
    else if inGarbage then
      s.head match
        case '>' => countGarbage(s.tail, acc, false, false)
        case '!' => countGarbage(s.tail, acc, true, true)
        case _   => countGarbage(s.tail, acc + 1, true, false)
    else
      println("bad!")
      1
  }
