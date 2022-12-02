package aoc

import munit.FunSuite

import scala.io.Source

class Day1Test extends FunSuite {
  test("day1") {
    val lines = Source.fromResource("aoc/day1.txt").getLines()
    println(Day1.maxGroup(lines))
  }

}
