package aoc

import aoc.Day3.{prioritize, solve, sumBadges}
import munit.FunSuite

import scala.io.Source

class Day3Test extends FunSuite {
  test("prioritize") {
    assertEquals(prioritize('a'), 1)
    assertEquals(prioritize('b'), 2)
    assertEquals(prioritize('z'), 26)
    assertEquals(prioritize('A'), 27)
    assertEquals(prioritize('B'), 28)
    assertEquals(prioritize('Z'), 52)
  }

  test("solve") {
    val resource = Source.fromResource("day3.txt").getLines()
    println(solve(resource))
  }

  test("sum badges") {
    val resource = Source.fromResource("day3.txt").getLines()
    println(sumBadges(resource))
  }

}
