package aoc

import aoc.Day8.scenicScore
import aoc.Day9.{solve, solve2}
import munit.FunSuite

import scala.io.Source

class Day9Test extends FunSuite {
  test("solve easy") {
    val input = """R 4
                  |U 4
                  |L 3
                  |D 1
                  |R 4
                  |D 1
                  |L 5
                  |R 2""".stripMargin.split("\n").toList
    assertEquals(solve(input), 13)
  }

  test("solve") {
    val resource = Source.fromResource("day9.txt").getLines().toList
    assertEquals(solve(resource), 5907)
  }

  test("solve 2") {
    val resource = Source.fromResource("day9.txt").getLines().toList
    println(solve2(resource))
  }


}
