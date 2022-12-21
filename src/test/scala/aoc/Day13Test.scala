package aoc

import munit.FunSuite
import Day13._

class Day13Test extends FunSuite {

  test("tokenizer") {
    assertEquals(tokenize("[[]],123,345"), List("[", "[", "]", "]", "123", "345"))
    assertEquals(tokenize(""), List())
  }



}
