package aoc

import scala.util.matching.Regex

object Day5 {

  def solve(lines: List[String]) = {
    val stacks = readHeader(lines)
    val commands = readCommands(lines.drop(10))
    val updatedStacks = applyCommands(stacks, commands)

    updatedStacks.map(_.head).mkString
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

  def move(from: List[Char], to: List[Char]) = (from.tail, from.head :: to)

  def applyCommands(stacks: Vector[List[Char]], cmds: List[MoveCmd]) =
    cmds.foldLeft(stacks) { case (s, cmd) =>
      val from = s(cmd.from)
      val to = s(cmd.to)

      val (updatedFrom, updatedTo) = (1 to cmd.n).foldLeft((from, to)) { case ((from, to), _) => move(from, to) }

      s.updated(cmd.from, updatedFrom).updated(cmd.to, updatedTo)
    }

  def solve2(lines: List[String]) = {
    val stacks = readHeader(lines)
    val commands = readCommands(lines.drop(10))
    val updatedStacks = applyCommands2(stacks, commands)

    updatedStacks.map(_.head).mkString
  }

  def applyCommands2(stacks: Vector[List[Char]], cmds: List[MoveCmd]) =
    cmds.foldLeft(stacks) { case (s, cmd) =>
      val from = s(cmd.from)
      val to = s(cmd.to)

      val updatedFrom = from.drop(cmd.n)
      val updatedTo = from.take(cmd.n) ::: to

      s.updated(cmd.from, updatedFrom).updated(cmd.to, updatedTo)
    }
}
