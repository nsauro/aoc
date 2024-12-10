package y2024

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day09 extends App:

  case class File(id: Long, pos: Int, size: Int) {
    val value: Long = (pos until pos + size).map(_ * id).sum
  }

  case class Space(pos: Int, size: Int)


  val data = Source.fromResource("2024/9.data").getLines().mkString("").toArray.map(_.asDigit)

  val p1Start = System.currentTimeMillis()
  val part1Res = part1(0, data.length - 1, 0, 0, 0, data.last, 0, data.length / 2, false)
  println(s"ran in ${System.currentTimeMillis() - p1Start}")
  println(part1Res)

  val (files, spaces, _) = data.zipWithIndex.foldLeft((ListBuffer.empty[File], ListBuffer.empty[Space], 0)) {
    case ((fs, ss, pos), (size, i)) =>
      if i % 2 == 0 then //file
        (fs.appended(File(i/2, pos, size)), ss, pos + size)
      else
        (fs, ss.appended(Space(pos, size)), pos + size)
  }

  val filesSorted = files.sortBy(-_.id)
  val spacesSorted = spaces.sortBy(_.pos)

  val p2Start = System.currentTimeMillis()
  val part2Res = part2(filesSorted.toSeq, spacesSorted, ListBuffer.empty)
  println(s"part2 ran in ${System.currentTimeMillis() - p2Start}")
  println(part2Res)


  @tailrec
  def part1(blockHead: Int, //position from left most side of dense format
            fileHead: Int,  //position from right most side of dense format
            pos: Int,  //position in expanded
            acc: Long, //total checksum
            curSpaceRemaining: Int, //current space remaining in blockhead's space
            curFileSizeRemaining:Int,  //current file size remaining in right most
            blockHeadFileId: Long, //id of left most file
            fileHeadFileId: Long, //id of right most file
            begunProcessingFile: Boolean //haxor..to know if i need to tack on remaining elements
           ) : Long = {
    if blockHead == fileHead || blockHead > fileHead then
      val remainder =
        if begunProcessingFile then
          ((pos until pos + curFileSizeRemaining).map(_ * fileHeadFileId)).sum
        else
          0
      acc + remainder
    else
      //blockhead is pointing at a file..so update the acc and move on
      if blockHead % 2 == 0 then
        val newPos = pos + data(blockHead)
        val s = (pos until newPos).map(_ * blockHeadFileId).sum
        val newAcc = s + acc
        part1(
          blockHead + 1, //move head to next
          fileHead,
          newPos,
          newAcc,
          data(blockHead + 1), //next pointer is a space, so set this to that value since it needs to be filled
          curFileSizeRemaining,
          blockHeadFileId + 1,
          fileHeadFileId,
          begunProcessingFile
        )
      else if curSpaceRemaining == 0 then //blockhead is pointing at a space, but it's filled so move on
        part1(
          blockHead + 1, //space is empty, move blockhead up to next location
          fileHead,
          pos, //don't move, we already incremented from last iteration
          acc,
          0,
          curFileSizeRemaining,
          blockHeadFileId,
          fileHeadFileId,
          begunProcessingFile
        )
      else if curFileSizeRemaining == 0 then //we have space, but the current file block is done, so move left
        part1(
          blockHead, //space is empty, move blockhead up to next location
          fileHead - 2, //skip over space
          pos, //don't move, we still need to fill the current space
          acc,
          curSpaceRemaining,
          data(fileHead - 2), //file is full again
          blockHeadFileId,
          fileHeadFileId - 1, //new file,
          false
        )
      else //we have space in the current gap, and blocks in our current file, so move it
        val newAcc = acc + (pos * fileHeadFileId)
        part1(
          blockHead,
          fileHead,
          pos + 1, //just filled a spot so move up
          newAcc,
          curSpaceRemaining - 1,
          curFileSizeRemaining - 1,
          blockHeadFileId,
          fileHeadFileId,
          true
        )
  }

  def part2(files: Seq[File], spaces: ListBuffer[Space], processedFiles: ListBuffer[File]): Long = {
    if(files.isEmpty) then
      processedFiles.map(_.value).sum
    else
      val toProcess = files.head
      val i = spaces.indexWhere{ x =>
        x.pos < toProcess.pos && x.size >= toProcess.size
      }
      if i == -1 then //no space suitable, leave file
        part2(files.tail, spaces, processedFiles.appended(toProcess))
      else
        val s = spaces(i)
        val updatedFile = toProcess.copy(pos = s.pos)
        val updatedSpace = s.copy(pos = s.pos + toProcess.size, size = s.size - toProcess.size)
        if updatedSpace.size == 0 then
          spaces.remove(i)
          part2(files.tail, spaces, processedFiles.appended(updatedFile))
        else
          spaces(i) = updatedSpace
          part2(files.tail, spaces, processedFiles.appended(updatedFile))
  }

