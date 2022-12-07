package aoc

import scala.util.matching.Regex

object Day5 {

  def readInput(lines: List[String]) = {
    val stacks = readHeader(lines)
    val commands = readCommands(lines.drop(10))
    println(commands)
  }

  def readHeader(lines: List[String]): Vector[List[Char]]= {
    val emptyStacks = Vector.fill(9)(List.empty[Char])

    val charAndStack = for {
      lineNum <- 7 to 0 by -1
      line = lines(lineNum)
      stackNum <- 0 to 8
      col =  stackNum * 4 + 1
      char = line.charAt(col)
    } yield (char, stackNum)

    charAndStack.foldLeft(emptyStacks) {
      case (s, (c, i)) => if (c != ' ') s.updated(i, c :: s(i)) else s
    }
  }

  case class MoveCmd(n: Int, from: Int, to: Int)

  private val cmdPattern: Regex = """move (\d+) from (\d+) to (\d+)""".r
  def readCommands(ss: List[String]): List[MoveCmd] = for {
    s <- ss
    cmdPattern(n, from, to) = s
  } yield MoveCmd(n.toInt, from.toInt -1, to.toInt -1)

  def applyCommands(stacks: Vector[List[Char]], cmds: List[MoveCmd]) = {
    cmds.foldLeft(stacks) { case (s, cmd) =>
      ???


    }
  }

}
