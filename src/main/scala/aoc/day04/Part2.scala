package aoc.day04

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

Now find one that starts with six zeroes.
 */
  import Part1.mineAdventCoins
  
  val input = IO.getLines().mkString  
  println(s"The key is: ${mineAdventCoins(input, _.startsWith("000000"))}")
}