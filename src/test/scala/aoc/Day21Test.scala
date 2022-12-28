package aoc

import munit.FunSuite
import Day21._

import scala.io.Source

class Day21Test extends FunSuite {
  test("parse") {
    val given = "root: pppw + sjmn"
    println(parseLine(given))
  }

  test("sample solve") {
    val given = """root: pppw + sjmn
                  |dbpl: 5
                  |cczh: sllz + lgvd
                  |zczc: 2
                  |ptdq: humn - dvpt
                  |dvpt: 3
                  |lfqf: 4
                  |humn: 5
                  |ljgn: 2
                  |sjmn: drzm * dbpl
                  |sllz: 4
                  |pppw: cczh / lfqf
                  |lgvd: ljgn * ptdq
                  |drzm: hmdt - zczc
                  |hmdt: 32""".stripMargin.split('\n').toList
    val monkeys = parse(given)
//    println(solve2(monkeys))
    assertEquals(solve(monkeys), 152L)
  }

  test("solve") {
    val file = Source.fromResource("day21.txt").getLines().toList
    val monkeys = parse(file)
    println(solve(monkeys))

  }

  test("solve2") {
    val file = Source.fromResource("day21.txt").getLines().toList
    val monkeys = parse(file)
    println(solve2(monkeys))

  }
}
