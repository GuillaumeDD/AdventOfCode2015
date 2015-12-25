package aoc.day01

import io.IO

object Part2 extends App {
  /*  --- Part Two ---
Now, given the same instructions, find the position of the first character that causes him to 
enter the basement (floor -1). The first character in the instructions has position 1, the 
second character has position 2, and so on.

For example:

    ) causes him to enter the basement at character position 1.
    ()()) causes him to enter the basement at character position 5.

What is the position of the character that causes Santa to first enter the basement?
   */
  def enterBasement(road: List[Char], floor: Int = 0, position: Int = 1): Int =
    {
      val (nextRoad, newFloor) = (road: @unchecked) match {
        case '(' :: remainingRoad =>
          (remainingRoad, floor + 1)
        case ')' :: remainingRoad =>
          (remainingRoad, floor - 1)
      }

      if (newFloor == -1) {
        position
      } else {
        enterBasement(nextRoad, newFloor, position + 1)
      }
    }

  val input = IO.getLines().mkString.toList

  println(s"Santa enters basement at position: ${enterBasement(input.toList)}")
}