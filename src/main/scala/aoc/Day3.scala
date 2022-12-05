package aoc

object Day3 {
  def prioritize(c: Char): Int = c match {
    case c if c.isLower => c.toInt - 'a'.toInt + 1
    case c if c.isUpper => c.toInt - 'A'.toInt + 27
  }

  def findCommonItem(s: String): Char = {
    val (first, second) = s.splitAt(s.length / 2)
    first.toSet.intersect(second.toSet).head
  }

  def solve(sacks: Iterator[String]): Int = sacks.map(findCommonItem).map(prioritize).sum

  def intersectAll(xs: Seq[String]): Set[Char] = xs.map(_.toSet).reduce(_ intersect _)

  def sumBadges(sacks: Iterator[String]) = sacks.sliding(3, 3)
    .map(intersectAll(_).head)
    .map(prioritize)
    .sum
}
