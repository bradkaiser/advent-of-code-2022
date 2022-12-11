package aoc

object Day9 {
  case class Point(x: Int, y: Int)
  case class Bridge(head: Point, tail: Point)

  sealed abstract class Cmd(val dx: Int, val dy: Int)
  case object Up extends Cmd(0, 1)
  case object Down extends Cmd(0, -1)
  case object Right extends Cmd(1, 0)
  case object Left extends Cmd(-1, 0)

  object Cmd {
    val fromLetter = Map("U" -> Up, "D" -> Down, "R" -> Right, "L" -> Left)
    def apply(s: String): Cmd = fromLetter(s)
  }

  val regex = """(\w) (\d+)""".r
  def parseLine(s: String): Seq[Cmd] = {
    val s"${direction} ${count}" = s
    val cmd = Cmd(direction)
    List.fill(count.toInt)(cmd)
  }

  def updateHead(head: Point, cmd: Cmd): Point = Point(head.x + cmd.dx, head.y + cmd.dy)

  def updateTail(head: Point, tail: Point) = {
    val dx = head.x - tail.x
    val dy = head.y - tail.y

    if (dx.abs >= 2 || dy.abs >= 2) Point(tail.x + dx.sign, tail.y + dy.sign) else tail
  }

  def solve(cmdStrings: Seq[String]) = cmdStrings.flatMap(parseLine)
      .scanLeft(Bridge(Point(0,0), Point(0,0))) { case (Bridge(head, tail), cmd) =>
        val newHead = updateHead(head, cmd)
        val newTail = updateTail(newHead, tail)
        Bridge(newHead, newTail)
      }
      .map(_.tail)
      .toSet
      .size

  case class LongTail(points: Vector[Point])
  case class Rope(head: Point, longTail: LongTail)

  def updateTail2(head: Point, lt: LongTail): LongTail =
    LongTail(lt.points.scanLeft(head) { (x, y) => updateTail(x, y) }.drop(1))

  def solve2(cmdStrings: Seq[String]) = cmdStrings.flatMap(parseLine)
    .scanLeft(Rope(Point(0,0), LongTail(Vector.fill(9)(Point(0,0))))) { case (Rope(head, tail), cmd) =>
      val newHead = updateHead(head, cmd)
      val newTail = updateTail2(newHead, tail)
      Rope(newHead, newTail)
    }
    .map(_.longTail.points(8))
    .toSet
    .size
}
