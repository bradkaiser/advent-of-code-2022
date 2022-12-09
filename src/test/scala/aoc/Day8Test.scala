package aoc

import aoc.Day8.{Tree, parse, scenicScore}
import munit.FunSuite

import scala.io.Source

class Day8Test extends FunSuite {

  test("parse") {
    val resource = Source.fromResource("day8.txt")
    val result = parse(resource.getLines().toList)
    println(result(0)(0))
  }

  test("get maxes") {
    println(Day8.getMaxes(List(Tree(0,1,0), Tree(0,2, 1), Tree(0,3, 1), Tree(0,4,2))))
  }

  test("find visible trees") {
    val resource = Source.fromResource("day8.txt")
    val result = Day8.parse(resource.getLines().toList)
    val test = """30373
                 |25512
                 |65332
                 |33549
                 |35390""".stripMargin.split("\n").toList
    assertEquals(Day8.findVisibleTrees(parse(test)), 21)

  }
  test("find visible trees full") {
    val resource = Source.fromResource("day8.txt")
    val result = Day8.parse(resource.getLines().toList)
    println(Day8.findVisibleTrees(result))

  }

  test("find scenic score") {
    val test = """30373
                 |25512
                 |65332
                 |33549
                 |35390""".stripMargin.split("\n").toList
    assertEquals(scenicScore(parse(test)), 8)
  }

  test("find scenic trees full") {
    val resource = Source.fromResource("day8.txt")
    val result = Day8.parse(resource.getLines().toList)
    assertEquals(scenicScore(result), 259308)

  }

}
