package aoc

import scala.collection.mutable

object Day13 {

  def tokenize(s: String): List[String] = {
    val result = mutable.ListBuffer[String]()

    var i = 0
    while (i < s.size) {
      s.charAt(i) match {
        case '[' => result += "["
        case ']' => result += "]"
        case ',' => //
        case x if x.isDigit =>
          val builder = new mutable.StringBuilder()
          while (i < s.size && s.charAt(i).isDigit) {
            builder.addOne(s.charAt(i))
            i += 1
          }
          result += builder.toString()
      }
      i += 1
    }

    result.toList
  }

  sealed trait Signal
  case class ListNode(xs: Vector[Signal]) extends Signal
  case class IntNode (x: Int) extends Signal

//  def parse(ss: List[String]): (Signal, List[String]) = {
//    val head :: remaining = ss
//    if (head.forall(Character.isDigit)) {
//      (IntNode(head.toInt), remaining)
//    } else if (head == "[") {
//      parseList(remaining, Vector.empty)
//    } else {
//      throw new RuntimeException("oh no")
//    }
//  }
//
//  def parseList(ss: List[String], acc: Vector[Nothing]): (Signal, List[String]) = ss match {
//    case "]" :: remaining => (ListNode(acc), remaining)
//    case list => parseList(remaining, acc :+ parse(head))
//
//  }


}
