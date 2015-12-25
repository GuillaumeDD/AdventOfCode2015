package aoc.day11

import io.IO

object Part2 extends App {
  import PasswordGenerator.increment
  
  val input = IO.getLines().mkString
  val firstPassword = increment(input)
  println(s"Next password of Santa (given: $firstPassword): ${increment(firstPassword)}")
}