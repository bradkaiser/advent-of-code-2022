
val boo = List(1)

boo match {
  case x :: remaining => println(remaining)
}