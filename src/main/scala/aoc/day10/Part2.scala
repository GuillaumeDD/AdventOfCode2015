package aoc.day10

import io.IO

object Part2 extends App {
  import Part1.lookAndSay

  val input = IO.getLines().mkString.toList
  val computation2 = (1 to 50).foldLeft(input) {
    (acc, i) =>
      lookAndSay(acc)
  }
  println(s"Length of the result: ${computation2.size}")
}