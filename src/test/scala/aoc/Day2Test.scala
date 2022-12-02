package aoc

import aoc.Day2.Hand.vs
import aoc.Day2.{Rock, score}
import munit.FunSuite

import scala.io.Source

class Day2Test extends FunSuite {

  val lines = Source.fromResource("day2.txt").getLines().toList

  test("test vs") {
    println(vs(Rock, Rock))
  }

//  test("check vs") {
//    assertEquals(score("A Y"), 8)
//    assertEquals(score("B X"), 1)
//  }

  test("score all") {
    assertEquals(Day2.scoreAll(lines), 15691)
  }

}
