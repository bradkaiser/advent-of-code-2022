package aoc

object Day4 {

  case class ElfPairAssignment(leftLo: Int, leftHi: Int, rightLo: Int, rightHi: Int) {
    def fullyOverlaps: Boolean = (leftLo >= rightLo && leftHi <= rightHi) || (rightLo >= leftLo && rightHi <= leftHi)
    def partialOverlap: Boolean = (leftLo >= rightLo && leftLo <= rightHi) || (leftHi >= rightLo && leftHi <= rightHi)
    def anyOverlap: Boolean = partialOverlap || fullyOverlaps
  }

  def parse(s: String):ElfPairAssignment = {
    val Array(Array(ll, lh), Array(rl, rh)) = s.split(',').map(c => c.split('-').map(_.toInt))
    ElfPairAssignment(ll, lh, rl, rh)
  }

  def findAllFullOverlaps(xs: Iterator[String]) = xs.map(parse).filter(_.fullyOverlaps).size

  def findAllAnyOverlap(xs: Iterator[String]): Int = xs.map(parse).filter(_.anyOverlap).size
}
