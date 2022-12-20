package aoc

object Day12 {
  case class Point(r: Int, c: Int)

  def findStartAndEnd(ss: List[String]): (Point, Point) = {
    var start: Point = null
    var end: Point = null
    for {
      r <- 0 until ss.size
      c <- 0 until ss.head.size
    } {
      if (ss(r).charAt(c) == 'S') start = Point(r, c)
      if (ss(r).charAt(c) == 'E') end = Point(r, c)
    }

    (start, end)
  }

  def parse(ss: List[String]): Vector[Vector[Int]] = {
    ss.map { s =>
      s.map { c =>
        val cleaned = if (c == 'S') 'a' else if (c == 'E') 'z' else c
        cleaned.toInt - 'a'.toInt
      }.toVector
    }.toVector
  }

  def solve(start: Point, end: Point, map: Vector[Vector[Int]]) = {
    val dirs = List((1,0), (-1,0), (0,1), (0,-1))
    val rows = map.size
    val cols = map.head.size

    def bfs(queue: Vector[(Point, Int)], seen: Set[Point]): Int = queue match {
      case Vector() => -1
      case (current, count) +: remaining if current == end => count
      case (current, count) +: remaining if seen.contains(current) => bfs(remaining, seen)
      case (current, count) +: remaining =>
        val newPoints = dirs
          .map {case (dr, dc) => Point(current.r + dr, current.c + dc)}
          .filter(p => p.r >= 0 && p.r < rows && p.c >= 0 && p.c < cols)
          .filterNot(seen.contains)
          .filter(p => map(p.r)(p.c) <= map(current.r)(current.c) + 1)
          .map(p => (p, count+1))

        println(s"$current $count => $newPoints, seen = $seen")

        bfs(remaining :++ newPoints, seen + current)
    }

    bfs(Vector((start, 0)), Set.empty)
  }

  def solve2(end: Point, map: Vector[Vector[Int]]) = {
    val dirs = List((1,0), (-1,0), (0,1), (0,-1))
    val rows = map.size
    val cols = map.head.size

    def bfs(queue: Vector[(Point, Int)], seen: Set[Point]): Int = queue match {
      case Vector() => Int.MaxValue
      case (current, count) +: remaining if current == end => count
      case (current, count) +: remaining if seen.contains(current) => bfs(remaining, seen)
      case (current, count) +: remaining =>
        val newPoints = dirs
          .map {case (dr, dc) => Point(current.r + dr, current.c + dc)}
          .filter(p => p.r >= 0 && p.r < rows && p.c >= 0 && p.c < cols)
          .filterNot(seen.contains)
          .filter(p => map(p.r)(p.c) <= map(current.r)(current.c) + 1)
          .map(p => (p, count+1))


        bfs(remaining :++ newPoints, seen + current)
    }

    (for {
      r <- 0 until rows
      c <- 0 until cols if map(r)(c) == 0
      p = Point(r, c)
      score = bfs(Vector((p, 0)), Set.empty)
    } yield score).min
  }

}
