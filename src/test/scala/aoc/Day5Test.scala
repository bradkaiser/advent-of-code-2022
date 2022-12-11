package aoc

import munit.FunSuite

import scala.io.Source

class Day5Test extends FunSuite {

  test("solve") {
    val lines = Source.fromResource("day5.txt").getLines().toList
    println(Day5.solve(lines))
  }

  test("solve2") {
    val lines = Source.fromResource("day5.txt").getLines().toList
    println(Day5.solve2(lines))
  }
}
