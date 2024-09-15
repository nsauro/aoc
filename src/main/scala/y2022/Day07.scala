package y2022

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day07 extends App {

  val data = Source.fromResource("2022/Day07").getLines().toSeq

  val CdCmd = raw"\$$ cd (.+)".r
  val LsCmd = raw"\$$ ls".r
  val DirDesc = raw"dir (.+)".r
  val FileDesc = raw"(\d+) (.+)".r

  val root = Dir("/", ListBuffer.empty, None)
  compute(data.tail, root)
  println(root.sumDirectoriesSizeLessThan(100000))

  val freeSpace = 70000000 - root.size
  val neededSpace = 30000000 - freeSpace
  println(s": freespace: $freeSpace")
  println(s": neededspace: $neededSpace")

  println(root.findSmallestDirectoryGreaterThanOrEqual(neededSpace))

  def compute(instr: Seq[String], curDir: Dir): Unit = {
    if (instr.nonEmpty) {
      instr.head match {
        case CdCmd("..") => compute(instr.tail, curDir.parent.get)
        case CdCmd(dir)  => compute(instr.tail, curDir.getDirectory(dir))
        case LsCmd()     => compute(instr.tail, curDir)
        case DirDesc(name) => {
          curDir.contents.addOne(Dir(name, ListBuffer.empty, Some(curDir)))
          compute(instr.tail, curDir)
        }
        case FileDesc(size, name) => {
          curDir.contents.addOne(File(name, size.toInt))
          compute(instr.tail, curDir)
        }
      }
    }
  }

  sealed trait Node {
    def size: Int
  }

  case class Dir(name: String, contents: ListBuffer[Node], parent: Option[Dir])
      extends Node {
    def size = contents.map(_.size).sum

    def getDirectory(name: String) = contents.collectFirst {
      case a @ Dir(n, _, _) if n == name => a
    }.get

    override def toString: String = s"name: $name"

    def sumDirectoriesSizeLessThan(maxSize: Int): Int = {
      val start = if (this.size <= maxSize) size else 0
      contents.foldLeft(start) {
        case (acc, a: Dir) => (acc + a.sumDirectoriesSizeLessThan(maxSize))
        case (acc, _)      => acc
      }
    }

    def findSmallestDirectoryGreaterThanOrEqual(minSize: Int): Int = {
      val start = if (this.size >= minSize) size else Integer.MAX_VALUE
      contents.foldLeft(start) {
        case (acc, a: Dir) =>
          Math.min(acc, a.findSmallestDirectoryGreaterThanOrEqual(minSize))
        case (acc, _) => acc
      }
    }
  }

  case class File(name: String, size: Int) extends Node

}
