package aoc

object Day2 {

  sealed abstract class Hand(val score: Int)
  case object Rock extends Hand(1)
  case object Paper extends Hand(2)
  case object Scissors extends Hand(3)

  def hand(c: Char): Hand = c match {
    case 'A' | 'X' => Rock
    case 'B' | 'Y' => Paper
    case 'C' | 'Z' => Scissors
  }

  def vs(yours: Hand, theirs: Hand): Int = (yours, theirs) match {
    case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 0
    case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => 6
  }

  def parseLine(s: String): (Hand, Hand) = (hand(s.charAt(0)), hand(s.charAt(2)))
  def score(theirs: Hand, yours: Hand): Int = vs(yours, theirs) + yours.score
  def scoreAll(lines: List[String]): Int = lines.map(parseLine).map((score _).tupled).sum
}
