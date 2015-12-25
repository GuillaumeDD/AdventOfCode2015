package io

object IO {
  /**
   * Read from the standard input line by line and apply a transformation for each line.
   * @param f Transformation function
   *
   */
  private def identity(s: String): String = s
  def getLines[T](f: String => T = identity(_)): List[T] =
    scala.io.Source.stdin.getLines().map(f).toList
}