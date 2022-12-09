package aoc

import aoc.Day7.{Cd, CdUp, Dir, DirLine, FileLine, Ls, build}
import munit.FunSuite

import scala.io.Source

class Day7Test extends FunSuite {
  test("parse") {
    assertEquals(Day7.parseLine("$ cd boo"), Cd("boo"))
    assertEquals(Day7.parseLine("$ cd .."), CdUp)
    assertEquals(Day7.parseLine("$ ls"), Ls)
    assertEquals(Day7.parseLine("dir a"), DirLine("a"))
    assertEquals(Day7.parseLine("123 boo.txt"), FileLine(123, "boo.txt"))
  }

  test("build") {
    val resource = Source.fromResource("day7.txt")
    val parsed = resource.getLines.take(19).toList.map(Day7.parseLine)
    println(build(parsed))

  }

}
