package aoc

object Day8 {

  def parse(lines: List[String]): Vector[Vector[Int]] = lines.map(s => s.map(_ - 0.toInt).toVector).toVector

  case class Tree(r: Int, c: Int, value: Int)
  case class State(currentMax: Int, maxes: List[Tree])

  def getMaxes(xs: Seq[Tree]): List[Tree] = {
    val finalState = xs.foldLeft(State(-1, Nil)) { case (state, tree) =>
      if (tree.value > state.currentMax) {
        State(tree.value, tree :: state.maxes)
      } else {
        state
      }
    }

    finalState.maxes
  }

  def findVisibleTrees(trees: Vector[Vector[Int]]) = {
    val rows = trees.length
    val cols = trees(0).length

    val leftRight = (0 until rows).map { r =>
      (0 until cols).map(c => Tree(r,c, trees(r)(c)))
    }.flatMap(getMaxes)

    val rightLeft = (0 until rows).map { r =>
      (cols -1 to 0 by -1).map(c => Tree(r,c, trees(r)(c)))
    }.flatMap(getMaxes)

    val topBottom = (0 until cols).map { c =>
      (0 until rows).map(r => Tree(r,c, trees(r)(c)))
    }.flatMap(getMaxes)

    val bottomTop = (0 until cols).map { c =>
      (rows - 1 to 0 by -1).map(r => Tree(r,c, trees(r)(c)))
    }.flatMap(getMaxes)

    val allVisible = leftRight.toSet ++ rightLeft.toSet ++ topBottom.toSet ++ bottomTop.toSet
    allVisible.size
  }

  def march(trees: Vector[Vector[Int]], origHeight: Int, r: Int, c: Int, dr: Int, dc: Int, acc: Int): Int = {
    val newR = r + dr
    val newC = c + dc

    if (newR < 0 || newR >= trees.length || newC < 0 || newC >= trees(0).length) {
      acc
    } else if (trees(newR)(newC) >= origHeight) {
      acc+1
    } else {
      march(trees, origHeight, newR, newC, dr, dc, acc+1)
    }
  }

  def scenicScore(trees: Vector[Vector[Int]]) = (for {
      r <- 0 until trees.length
      c <- 0 until trees(0).length
      right = march(trees, trees(r)(c), r, c, 0, 1, 0)
      left = march(trees, trees(r)(c), r, c, 0, -1, 0)
      down = march(trees, trees(r)(c), r, c, 1, 0, 0)
      up = march(trees, trees(r)(c), r, c, -1, 0, 0)
    } yield right * left * down * up).max
}
