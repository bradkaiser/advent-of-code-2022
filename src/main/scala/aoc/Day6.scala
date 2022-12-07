package aoc

object Day6 {

  def allUnique(s: Seq[Char]): Boolean = s.size == s.toSet.size

  def findStart(s: Iterator[Char]) = {
    s.sliding(4,1).zipWithIndex.find { case (s, _) => allUnique(s)}.map { case (s, i) => i + 4}
  }

  def findStart2(s: Iterator[Char]) = {
    s.sliding(14,1).zipWithIndex.find { case (s, _) => allUnique(s)}.map { case (s, i) => i + 14}
  }
}
