package aoc

import munit.FunSuite
import Day13._

import scala.io.Source

class Day13Test extends FunSuite {

  test("parse") {
    assertEquals(parse("[1,[2,3,4],5]".toList)._1, ListNode(
      IntNode(1),
      ListNode(IntNode(2),IntNode(3),IntNode(4)),
      IntNode(5))
    )
  }

  test("compare") {
    import Day13.{IntNode => IN, ListNode => LN}
    assertEquals(compare(IN(1), IN(2)), -1)
    assertEquals(compare(IN(2), IN(1)), 1)
    assertEquals(compare(IN(2), IN(2)), 0)
    assertEquals(compare(LN(IN(1), IN(2)), LN(IN(1), IN(2))), 0)
    assertEquals(compare(LN(IN(0), IN(2)), LN(IN(1), IN(2))), -1)
    assertEquals(compare(LN(IN(1), IN(3)), LN(IN(1), IN(2))), 1)
    assertEquals(compare(LN(IN(1)), LN(IN(1), IN(2))), -1)
  }

  test("solve") {
    val lines = Source.fromResource("day13.txt").getLines()
    println(solve(lines))

  }
  test("solve2") {
    val lines = Source.fromResource("day13.txt").getLines()
    println(solve2(lines))

  }



}
