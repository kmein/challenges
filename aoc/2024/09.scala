import scala.io.Source
object Aoc09 {
  case class Memory(bs: List[Option[Int]]) {
    var blocks = bs

    def debug() = {
      this.blocks.map(x => x match {
        case None => "."
        case Some(x) => if (x > 9) "+" else x.toString
      }).foreach(print)
      println()
    }

    def defragment(): Memory = {
      def go(blocksFragmented: List[Option[Int]], acc: List[Option[Int]]): Memory = {
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

    def checksum(): Int = blocks.zipWithIndex.map{case (c, i) => c.getOrElse(0) * i}.sum
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

  def parseBlocks(input: String): List[Option[Int]] = input.zipWithIndex.toList.flatMap { case (blockSize, i) =>
    List.fill(blockSize.asDigit)(if (i % 2 == 0) Some(i/2) else None)
  }

  def main(args: Array[String]): Unit = {
    val memory = Memory(parseBlocks(readInput()))
    memory.debug()

    val defragmented = memory.defragment()
    defragmented.debug()

    println(defragmented.checksum())
  }
}
