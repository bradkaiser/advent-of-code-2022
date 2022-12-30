package aoc

import java.text.ParseException
import scala.collection.mutable

object Day13 {
  sealed trait Signal
  case class ListNode(xs: Vector[Signal]) extends Signal
  object ListNode {
    def apply(ss: Signal*): ListNode = {
      ListNode(Vector.from(ss))
    }
  }
  case class IntNode (x: Int) extends Signal

  def parse(s: String): Signal = parse(s.toList)._1

  def parse(cs: List[Char]): (Signal, List[Char]) = cs match {
    case '[' :: remain => parseList(Vector.empty, remain)
    case c :: remain if c.isDigit => parseNumber(Vector(c), remain)
    case _ => throw new RuntimeException("can't parse")
  }

  def parseList(acc: Vector[Signal], cs: List[Char]): (Signal, List[Char]) = cs match {
    case ']' :: remaining => (ListNode(acc), remaining)
    case ',' :: remaining => parseList(acc, remaining)
    case _ =>
      val (node, remaining) = parse(cs)
      parseList(acc :+ node, remaining)
  }

  def parseNumber(acc: Vector[Char], cs: List[Char]): (Signal, List[Char]) = cs match {
    case c :: remaining if c.isDigit => parseNumber(acc :+ c, remaining)
    case ',' :: remaining => (IntNode(acc.mkString.toInt), remaining)
    case _ => (IntNode(acc.mkString.toInt), cs)
  }

  // -1 if left is smaller, 0 if equal, 1 if left is bigger
  def compare(left: Object, right: Object): Int = (left, right) match {
    case (IntNode(x), IntNode(y)) => x compare y
    case (x: IntNode, y: ListNode) => compare(ListNode(x), y)
    case (x: ListNode, y: IntNode) => compare(x, ListNode(y))
    case (ListNode(xs), ListNode(ys)) => xs.zip(ys)
      .map{case (x,y) => compare(x,y)}
      .find(_ != 0)
      .getOrElse(xs.size compare ys.size)
  }

  def solve(ss: Iterator[String]) = {
    ss.sliding(2,3)
      .map { case Seq(left, right) => (parse(left), parse(right))}
      .map((compare _).tupled)
      .zipWithIndex
      .filter { case (r, i) => r == -1}
      .map{ case (_, i) => i +1}
      .sum
  }

  def solve2(ss: Iterator[String]): Int = {
    val sorted = ss.filterNot(_.isEmpty)
      .map(parse)
      .toList
      .prepended(ListNode(ListNode(IntNode(2))))
      .prepended(ListNode(ListNode(IntNode(6))))
      .sortWith(compare(_,_) < 0)
      .zipWithIndex

    sorted.collect {
      case (ListNode(Vector(ListNode(Vector(IntNode(2))))), i) => i + 1
      case (ListNode(Vector(ListNode(Vector(IntNode(6))))), i) => i + 1
    }.product
  }


}
