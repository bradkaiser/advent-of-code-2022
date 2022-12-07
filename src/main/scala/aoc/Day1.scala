package aoc

object Day1 {

  def partitionBy[A](xs: Iterator[A])(pred: A => Boolean): Iterator[Iterator[A]] = new Iterator[Iterator[A]] {
    override def hasNext: Boolean = xs.hasNext
    override def next(): Iterator[A] = xs.takeWhile(pred)
  }

  def maxGroup(lines: Iterator[String]) = partitionBy(lines)(!_.isEmpty).map(chunk => chunk.map(_.toInt).sum).max

  def maxGroup3(lines: Iterator[String]) = partitionBy(lines)(!_.isEmpty).map(chunk => chunk.map(_.toInt).sum)
    .toList
    .sorted
    .reverse
    .take(3)
    .sum
}
