object Quine {
  def main(args: Array[String]) = {
    val sourceCode = "object Quine {\n  def main(args: Array[String]) = {\n    val sourceCode = %s\n    printf(sourceCode, sourceCode.stripMargin)\n  }\n}"
    printf(sourceCode, sourceCode.stripMargin)
  }
}


  Quine.main(Array())