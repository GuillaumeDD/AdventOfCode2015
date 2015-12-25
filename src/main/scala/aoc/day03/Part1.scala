package aoc.day03

import io.IO

object Part1 extends App {
  /*
--- Day 3: Perfectly Spherical Houses in a Vacuum ---

Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and 
then an elf at the North Pole calls him via radio and tells him where to move next. 
Moves are always exactly one house to the north (^), south (v), east (>), or west (<).
After each move, he delivers another present to the house at his new location.

However, the elf back at the north pole has had a little too much eggnog, and so his
directions are a little off, and Santa ends up visiting some houses more than once. 
How many houses receive at least one present?

For example:

    > delivers presents to 2 houses: one at the starting location, and one to the east.
    ^>v< delivers presents to 4 houses in a square, including twice to the house at his 
         starting/ending location.
    ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
 */

  type House = (Int, Int)

  def computeNewPosition(d: Char, pos: House): House =
    d match {
      case '^' =>
        val (x, y) = pos
        (x, y + 1)
      case '<' =>
        val (x, y) = pos
        (x - 1, y)
      case 'v' =>
        val (x, y) = pos
        (x, y - 1)
      case '>' =>
        val (x, y) = pos
        (x + 1, y)
    }

  def keepingTracksOfSanta(
    directions: List[Char],
    currentPosition: House = (0, 0),
    visitedHouse: Set[House] = Set((0, 0))): Set[House] =
    directions match {
      case d :: nextDirections =>
        val (x, y) = currentPosition
        val newPosition = computeNewPosition(d, currentPosition)
        keepingTracksOfSanta(nextDirections, newPosition, visitedHouse + newPosition)
      case List() => visitedHouse
    }

  val input = IO.getLines().mkString.toList

  println(s"Number of houses receiving at least one present: ${keepingTracksOfSanta(input).size}")

}