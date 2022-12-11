package aoc

object Day10 {

  def parseLine(s: String) = s match {
    case "noop" => List(0)
    case s"addx ${num}" => List (0,num.toInt)
  }

  def solve(ss: Vector[String])= {
    val signalStrengths = ss
      .flatMap(parseLine)
      .scanLeft(1)(_ + _)
      .zipWithIndex
      .map { case (register, cycle) => register * (cycle+1)}
    List(20, 60, 100, 140, 180, 220)
      .map(x => signalStrengths(x - 1))
      .sum
  }

  def solve2(ss: Vector[String]) = {
    val signalStrengths = ss
      .flatMap(parseLine)
      .scanLeft(1)(_ + _)
      .zipWithIndex
      .map { case (register, cycle) => (register, cycle % 40)}
      .map { case (register, x) => if ((register - x).abs <= 1 ) "#" else " "}
      .sliding(40, 40)
      .map(_.mkString)
      .foreach(println)

  }

}
