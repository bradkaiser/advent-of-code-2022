package aoc

object Day11 {
  case class Item(worry: Long)
  object Item {
    def parse(s: String): Vector[Item] = s.split(", ").map(_.toInt).map(Item(_)).toVector
  }
  case class Op(x: Term, y: Operation, z: Term) {
    def toInt(t: Term, old: Long): Long = t match {
      case Const(n) => n
      case Old => old
    }

    def update(worry: Long): Long = y.apply(toInt(x, worry), toInt(z, worry))
  }

  object Op {
    def parse(s: String): Op = {
      val s"${x} ${o} ${y}" = s
      Op(Term(x), Operation(o), Term(y))
    }
  }

  val intRegex= """(\d+)""".r
  sealed trait Term
  sealed trait Operation {
    def apply(x: Long, y: Long): Long
  }
  object Term {
    def apply(s: String) = s match {
      case "old" => Old
      case intRegex(n) => Const(n.toInt)
    }
  }

  object Operation {
    def apply(s: String) = s match {
      case "+" => Plus
      case "*" => Times
    }
  }

  case object Old extends Term
  case class Const(n: Long) extends Term

  case object Plus extends Operation {
    def apply(x: Long, y: Long): Long = x + y
  }
  case object Times extends Operation {
    def apply(x: Long, y: Long): Long = x * y
  }




  case class Monkey(items: Vector[Item], op: Op , test: Int, trueMonkeyIdx: Int, falseMonkeyIdx: Int, inspections: Long) {
    override def toString: String = s"""m(${items.map(_.worry).mkString(",")}; ${inspections})"""
  }

  object Monkey {
    val monkeyRegex = """Monkey .*:
                        |  Starting items: (.*)
                        |  Operation: new = (.*)
                        |  Test: divisible by (.*)
                        |    If true: throw to monkey (.*)
                        |    If false: throw to monkey (.*)""".stripMargin.r

    def apply(s: String): Monkey = {
      val monkeyRegex(itemS, opS, divS, trueS, falseS) = s
      Monkey(Item.parse(itemS), Op.parse(opS), divS.toInt, trueS.toInt, falseS.toInt, 0)
    }
  }


  def parse(lines: Iterator[String]) = lines
      .sliding(6, 7)
      .map(_.mkString("\n"))
      .map {Monkey(_)}
      .toVector

  def solve(lines: Iterator[String]) = {
    val originalMonkeys = parse(lines)

    val updatedMonkeys = (1 to 20).foldLeft(originalMonkeys) { case (monkeys, _) =>
      (0 until monkeys.size).foldLeft(monkeys) { case (monkeys, mIdx) =>
//        monkeys.zipWithIndex.foreach(println)
//        println("---")
        doMonkeyActions(monkeys, mIdx)
      }
    }

      updatedMonkeys.map(_.inspections).sorted.reverse.take(2).product
  }

  def doMonkeyActions(monkeys: Vector[Monkey], mIdx: Int): Vector[Monkey] = {
    val sourceMonkey = monkeys(mIdx)
    val trueDestMonkey = monkeys(sourceMonkey.trueMonkeyIdx)
    val falseDestMonkey = monkeys(sourceMonkey.falseMonkeyIdx)

    if (sourceMonkey.items.isEmpty) {
      monkeys
    } else {
      val Item(worry) +: remaining = sourceMonkey.items

      val newWorry = sourceMonkey.op.update(worry) / 3
      val updatedSourceMonkey = sourceMonkey.copy(items = remaining, inspections = sourceMonkey.inspections + 1)

      if (newWorry % sourceMonkey.test == 0) {
        val updatedTrueDestMonkey = trueDestMonkey.copy(items = trueDestMonkey.items.appended(Item(newWorry)))
        val updatedMonkeys = monkeys
          .updated(mIdx, updatedSourceMonkey)
          .updated(sourceMonkey.trueMonkeyIdx, updatedTrueDestMonkey)

        doMonkeyActions(updatedMonkeys, mIdx)
      } else {
        val updatedFalseDestMonkey = falseDestMonkey.copy(items = falseDestMonkey.items.appended(Item(newWorry)))
        val updatedMonkeys = monkeys
          .updated(mIdx, updatedSourceMonkey)
          .updated(sourceMonkey.falseMonkeyIdx, updatedFalseDestMonkey)

        doMonkeyActions(updatedMonkeys, mIdx)
      }
    }
  }

  def solve2(lines: Iterator[String]) = {
    val originalMonkeys = parse(lines)

    val updatedMonkeys = (1 to 10000).foldLeft(originalMonkeys) { case (monkeys, _) =>
      (0 until monkeys.size).foldLeft(monkeys) { case (monkeys, mIdx) =>
//                monkeys.zipWithIndex.foreach(println)
//                println("---")
        doMonkeyActions2(monkeys, mIdx)
      }
    }

    println(updatedMonkeys)
    updatedMonkeys.map(_.inspections).sorted.reverse.take(2).product
  }

  def doMonkeyActions2(monkeys: Vector[Monkey], mIdx: Int): Vector[Monkey] = {
    val supermodulo = monkeys.map(_.test).product
    val sourceMonkey = monkeys(mIdx)
    val trueDestMonkey = monkeys(sourceMonkey.trueMonkeyIdx)
    val falseDestMonkey = monkeys(sourceMonkey.falseMonkeyIdx)

    if (sourceMonkey.items.isEmpty) {
      monkeys
    } else {
      val Item(worry) +: remaining = sourceMonkey.items

      val newWorry = sourceMonkey.op.update(worry) % supermodulo
      val updatedSourceMonkey = sourceMonkey.copy(items = remaining, inspections = sourceMonkey.inspections + 1)

      if (newWorry % sourceMonkey.test == 0) {
        val updatedTrueDestMonkey = trueDestMonkey.copy(items = trueDestMonkey.items.appended(Item(newWorry)))
        val updatedMonkeys = monkeys
          .updated(mIdx, updatedSourceMonkey)
          .updated(sourceMonkey.trueMonkeyIdx, updatedTrueDestMonkey)

        doMonkeyActions2(updatedMonkeys, mIdx)
      } else {
        val updatedFalseDestMonkey = falseDestMonkey.copy(items = falseDestMonkey.items.appended(Item(newWorry)))
        val updatedMonkeys = monkeys
          .updated(mIdx, updatedSourceMonkey)
          .updated(sourceMonkey.falseMonkeyIdx, updatedFalseDestMonkey)

        doMonkeyActions2(updatedMonkeys, mIdx)
      }
    }
  }
}
