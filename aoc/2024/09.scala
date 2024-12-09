import scala.io.Source
object Aoc09 {
  type FileId = Long
  type FileSize = Long
  type MemoryFile = (FileSize, Option[FileId])

  case class Memory(bs: List[Option[FileId]]) {
    var blocks = bs

    def debug() = {
      this.blocks.map(x => x match {
        case None => "."
        case Some(x) => if (x > 9) ('9'.toByte + x).toChar.toString else x.toString
      }).foreach(print)
      println()
    }

    def defragment(): Memory = {
      def go(blocksFragmented: List[Option[FileId]], acc: List[Option[FileId]]): Memory = {
        println(blocksFragmented.length)

        blocksFragmented match {
          case Nil => Memory(acc.reverse)
          case a :: Nil => go(Nil, a :: acc)
          case rest :+ None => go(rest, acc)
          case None +: rest :+ Some(last) => go(rest, Some(last) :: acc)
          case Some(head) +: rest :+ Some(last) => go(rest :+ Some(last), Some(head) :: acc)
        }
      }

      go(this.blocks, List())
    }

    def defragmentImperative(): Memory = {
      var blocksArray: Array[Option[FileId]] = this.blocks.toArray

      for (i <- this.blocks.indices.reverse) {
        if (!(blocksArray(i) == None || blocksArray.indexOf(None) > i)) {
          blocksArray(blocksArray.indexOf(None)) = blocksArray(i)
          blocksArray(i) = None
        }
      }

      Memory(blocksArray.toList)
    }

    def checksum(): Long = blocks.zipWithIndex.map{case (c, i) => (c.getOrElse(0L) * i)}.sum
  }

  def readInput(): String = {
    val fileName = Option(System.getenv("AOC_TEST")) match {
      case Some(_) => "09.txt.test"
      case None => "09.txt"
    }
    try {
      val source = Source.fromFile(fileName)
      val firstLine = source.getLines().next().trim
      source.close()

      return firstLine
    } catch {
      case e: Exception => println(s"Error reading file: ${e.getMessage}")
      return ""
    }
  }

  def parseBlocks(input: String): List[Option[FileId]] = input.zipWithIndex.toList.flatMap { case (blockSize, i) =>
    List.fill(blockSize.asDigit)(if (i % 2 == 0) Some(i/2) else None)
  }

  def parseBlocksRLE(input: String): List[MemoryFile] = input.zipWithIndex.toList.map { case (blockSize, i) =>
    (blockSize.asDigit, if (i % 2 == 0) Some(i/2) else None)
  }

  def insertListAt[A](list: List[A], index: Int, newList: List[A]): List[A] = {
    // println("Inserting list " + newList + " at index " + index + " of " + list)
    val (taken, dropped) = list.splitAt(index)
    taken ::: newList ::: dropped.tail
  }

  def compact(memory: List[MemoryFile]): List[MemoryFile] = {
    def go(acc: List[MemoryFile], fields: List[(MemoryFile, Int)]): List[MemoryFile] = fields match {
      case ((size, Some(fileId)), index) :: tail => {
        // (toMemory(acc)).debug()
        acc.zipWithIndex.find{ case ((noneSize, v), _) => v == None && noneSize >= size } match {
          case Some(((noneSize, None), newIndex)) if newIndex < index => {
            val newList =
              if (noneSize == size)
                List((size, Some(fileId)))
              else
                List((size, Some(fileId)), (noneSize - size, None))
            go(
              insertListAt(
                acc.map(x => x match {
                  case (s, Some(oldFileId)) if fileId == oldFileId => (s, None)
                  case _ => x
                }),
                newIndex,
                newList
              ),
              tail
            )
          }
          case _ => go(acc, tail)
        }
      }
      case ((_, None), _) :: tail => go(acc, tail)
      case Nil => acc
    }

    go(memory, memory.zipWithIndex.reverse)
  }

  def toMemory(blocks: List[MemoryFile]): Memory = Memory(blocks.flatMap { case (blockSize, i) => List.fill(blockSize.toInt)(i) })

  def main(args: Array[String]): Unit = {
    val input = readInput()
    val blocksRLE = parseBlocksRLE(input)

    val memory = toMemory(blocksRLE)
    memory.debug()

    val defragmented = memory.defragmentImperative()
    defragmented.debug()

    println(defragmented.checksum())

    val blocksCompact = toMemory(compact(blocksRLE))
    blocksCompact.debug()
    println(blocksCompact.checksum())
  }
}
