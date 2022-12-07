package aoc

import munit.FunSuite

import scala.io.Source

class Day6Test extends FunSuite {
  test("solution") {
    val resource = Source.fromResource("day6.txt").iter
    println(Day6.findStart2(resource))
  }


}
