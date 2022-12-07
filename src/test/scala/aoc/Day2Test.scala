package aoc

import munit.FunSuite

import scala.io.Source

class Day2Test extends FunSuite {
  val lines = Source.fromResource("day2.txt").getLines().toList
  test("score all") {
    assertEquals(Day2.scoreAll(lines), 15691)
  }

  test("score2 all") {
    println(Day2.scoreAll2(lines))
  }
}
