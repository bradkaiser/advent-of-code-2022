package aoc

import aoc.Day4.{findAllAnyOverlap, findAllFullOverlaps}
import munit.FunSuite

import scala.io.Source

class Day4Test extends FunSuite {
  test("findAllOverlaps") {

    val resource = Source.fromResource("day4.txt").getLines()
    assertEquals(findAllAnyOverlap(resource), 874)

  }

}
