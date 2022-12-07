package aoc

object Day2 {

  sealed abstract class Hand(val score: Int, _beats: => Hand, _losesTo: => Hand) {
    lazy val beats = _beats
    lazy val losesTo = _losesTo
  }

  case object Rock extends Hand(1, Scissors, Paper)
  case object Paper extends Hand(2, Rock, Scissors)
  case object Scissors extends Hand(3, Paper, Rock)

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

  def parseLine2(s: String): (Hand, Char) = (hand(s.charAt(0)), s.charAt(2))
  def score2(theirs: Hand, c: Char): Int = c match {
    case 'X' => 0 + theirs.beats.score //lose
    case 'Y' => 3 + theirs.score //draw
    case 'Z' => 6 + theirs.losesTo.score // win
  }

  def scoreAll2(lines: List[String]): Int = lines.map(parseLine2).map((score2 _).tupled).sum
}

