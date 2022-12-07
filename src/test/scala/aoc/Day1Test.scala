package aoc

import munit.FunSuite

import scala.io.Source

class Day1Test extends FunSuite {
  test("day1") {
    val lines = Source.fromResource("day1.txt").getLines()
    println(Day1.maxGroup(lines))
  }

  test("day1 part 2") {
    val lines = Source.fromResource("day1.txt").getLines()
    println(Day1.maxGroup3(lines))
  }

}
