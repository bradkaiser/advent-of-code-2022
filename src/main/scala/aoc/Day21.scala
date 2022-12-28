package aoc

object Day21 {
  sealed trait Monkey
  case class Const(n: Long) extends Monkey
  case class Func(s: String, t: String, f: (Long, Long) => Long) extends Monkey

  def parse(ss: List[String]): Map[String, Monkey] = ss.map(parseLine).toMap

  def parseLine(s: String): (String, Monkey) = {
    val Array(name, rh) = s.split(": ")
    val parsed = rh match {
      case s"${x} + ${y}" => Func(x, y, _ + _)
      case s"${x} - ${y}" => Func(x, y, _ - _)
      case s"${x} * ${y}" => Func(x, y, _ * _)
      case s"${x} / ${y}" => Func(x, y, _ / _)
      case n => Const(n.toLong)
    }
    (name, parsed)
  }

  def solve(m: Map[String, Monkey]) = {
    helper("root", m)._1
  }


  def helper(name: String, m: Map[String, Monkey]): (Long, Map[String, Monkey]) = {
    val monkey = m(name)
    monkey match {
      case Const(n) => (n, m)
      case Func(x, y, f) =>
        val (xSolution, newMap) = helper(x, m)
        val (ySolution, newNewMap) = helper(y, newMap)
        val newValue = f(xSolution, ySolution)
        (newValue, newNewMap + (name -> Const(newValue)))
    }
  }

  def solve2(m: Map[String, Monkey]) = {
    val root = m("root").asInstanceOf[Func]
    val left = root.s
    val right = root.t
    (0 to 100000).find(humn => helper2(left, m, humn)._1 == helper2(right, m, humn)._1)
  }

  def helper2(name: String, m: Map[String, Monkey], human: Long): (Long, Map[String, Monkey]) = {
    val monkey = if (name == "humn") Const(human) else  m(name)
    monkey match {
      case Const(n) => (n, m)
      case Func(x, y, f) =>
        val (xSolution, newMap) = helper2(x, m, human)
        val (ySolution, newNewMap) = helper2(y, newMap, human)
        val newValue = f(xSolution, ySolution)
        (newValue, newNewMap + (name -> Const(newValue)))
    }
  }
}
