package aoc

import munit.FunSuite

import scala.io.Source

class Day11Test extends FunSuite {
  test("parse") {
    val it = Source.fromResource("day11.txt").getLines()
    val result = Day11.parse(it)
    result.foreach(println)

  }

  test("solve") {
    val it = Source.fromResource("day11.txt").getLines()
    println(Day11.solve(it))
  }
  test("solve2") {
    val it = Source.fromResource("day11.txt").getLines()
    println(Day11.solve2(it))
  }
}
