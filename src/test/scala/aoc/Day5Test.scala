package aoc

import munit.FunSuite

import scala.io.Source

class Day5Test extends FunSuite {

  test("read config") {
    val lines = Source.fromResource("day5.txt").getLines().toList
    Day5.readInput(lines)

  }


}
