package aoc

import munit.FunSuite

import scala.io.Source

class Day12Test extends FunSuite {
  test("solve") {
    val lines = Source.fromResource("day12.txt").getLines().toList
    val (start, end) = Day12.findStartAndEnd(lines)
    val map = Day12.parse(lines)

    println(Day12.solve(start, end, map))
  }

  test("solve2") {
    val lines = Source.fromResource("day12.txt").getLines().toList
    val (start, end) = Day12.findStartAndEnd(lines)
    val map = Day12.parse(lines)

    println(Day12.solve2(end, map))
  }

}
