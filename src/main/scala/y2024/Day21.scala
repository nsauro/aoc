package y2024

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day21 extends App:
  val data = Source.fromResource("2024/21.data").getLines()

  val numPad = Array(
    Array('7', '8', '9'),
    Array('4', '5', '6'),
    Array('1', '2', '3'),
    Array('#', '0', 'A'),
  )

  val dirPad = Array(
    Array('#', '^', 'A'),
    Array('<', 'v', '>')
  )

  val paths = mutable.LinkedHashMap.empty[String, Seq[Char]]
  getAllPaths(numPad, paths)
  getAllPaths(dirPad, paths)

  val part1Res = data.map{s =>
    val num = s.init.toInt
    println(num)
    val first = genSeq("A" + s).mkString("")
    println(first)
    val second = genSeq("A" + first).mkString("")
    println(second)
    val third = genSeq("A" + second).mkString("")
    println(s"$s -- $third")
    third.length * num
  }.sum

  println(part1Res)


  /*paths.foreach(println)

  val first =  genSeq("A029A").mkString("")
  println(s"first: $first")
  val second = genSeq("A" + first).mkString("")
  println(s"second: $second")
  val third = genSeq("A" + second).mkString("")
  println(s"third: $third")

  println(first.length == "<A^A>^^AvvvA".length)
  println(second.length == "v<<A>>^A<A>AvA<^AA>A<vAAA>^A".length)
  println(third.length == "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".length)*/


  def getAllPaths(grid: Array[Array[Char]], acc: mutable.LinkedHashMap[String, Seq[Char]]): mutable.LinkedHashMap[String, Seq[Char]] = {
    val chars = grid.flatten.filter(_ != '#')
    for {
      from <- chars
      to <- chars if from != to
    } {

      val key = s"${from}${to}"
      val loc = findChar(0,0, from, grid)
      val path = bfs(to,
        ListBuffer(BfsPath(Seq(BfsPathEntry(loc, from, '-')))),
        grid)
      acc.addOne(key, path)
    }
    acc
  }


  def genSeq(s: String) : Seq[Char] = {
    s.sliding(2).flatMap(x => paths.getOrElse(x, Seq.empty) :+ 'A').toSeq
  }

  def bfs(c: Char,
           queue: ListBuffer[BfsPath],
           grid: Array[Array[Char]]) : Seq[Char] = {
    if queue.isEmpty then
      Seq.empty //?
    else
      val found = queue.filter(_.path.last.char == c)
      if found.size == 1 then
        found.head.path.tail.map(_.dir)
      else if found.nonEmpty then
        found.minBy{ p =>
          p.path.sliding(2).foldLeft(0){ case (acc, x) =>
            if(x.head.dir == x.last.dir) then
              acc + 0
            else
             acc + 1
          }
        }.path.tail.map(_.dir)
      else
        val nextQueue = ListBuffer.empty[BfsPath]
        queue.foreach { path =>
          val n = getNeighbors(path, grid)
          nextQueue.addAll(n)
        }
        bfs(c, nextQueue, grid)
  }

  def getNeighbors(
                    path: BfsPath,
                    grid: Array[Array[Char]]
                  ): ListBuffer[BfsPath] = {
    val neighbors = ListBuffer(
      ((path.last.loc._1 + 1, path.last.loc._2), '>'),
      ((path.last.loc._1 - 1, path.last.loc._2), '<'),
      ((path.last.loc._1, path.last.loc._2 + 1), 'v'),
      ((path.last.loc._1, path.last.loc._2 - 1), '^')
    )
    neighbors.filterNot(x =>
      x._1._1 < 0 || x._1._1 >= grid.head.length || x._1._2 < 0 || x._1._2 >= grid.length ||
        grid(x._1._2)(x._1._1) == '#' ||
      path.path.exists{ p =>
          p.loc == x._1
        }
    ).map{ l =>
      val c = grid(l._1._2)(l._1._1)
      path.copy(path = path.path :+ BfsPathEntry(l._1, c, l._2))
    }
  }

  case class BfsPath(path: Seq[BfsPathEntry]) {
    val last = path.last
  }
  case class BfsPathEntry(loc: (Int, Int), char: Char, dir: Char)

  @tailrec
  def findChar(x: Int, y: Int, char: Char, grid: Array[Array[Char]]): (Int, Int) = {
    if grid(y)(x) == char then
      (x, y)
    else if x + 1 == grid.head.length then
      findChar(0, y + 1, char, grid)
    else
      findChar(x + 1, y, char, grid)
  }