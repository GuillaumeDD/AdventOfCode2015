package aoc.day03

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

The next year, to speed up the process, Santa creates a robot version of himself, 
Robo-Santa, to deliver presents with him.

Santa and Robo-Santa start at the same location (delivering two presents to the 
same starting house), then take turns moving based on instructions from the elf, 
who is eggnoggedly reading from the same script as the previous year.

This year, how many houses receive at least one present?

For example:

    ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
    ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
    ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and 
               Robo-Santa going the other.
 */
  import Part1.{ House, computeNewPosition }

  def keepingTracksOfSantaAndRoboSanta(
    directions: List[Char],
    currentPositionOfSanta: House = (0, 0),
    currentPositionOfRoboSanta: House = (0, 0),
    visitedHouse: Set[House] = Set((0, 0)),
    movesSanta: Boolean = true): Set[House] = {

    directions match {
      case d :: nextDirections =>
        if (movesSanta) {
          // Santa
          val newPosition = computeNewPosition(d, currentPositionOfSanta)
          keepingTracksOfSantaAndRoboSanta(
            nextDirections,
            newPosition,
            currentPositionOfRoboSanta,
            visitedHouse + newPosition,
            false)
        } else {
          // RoboSanta
          val newPosition = computeNewPosition(d, currentPositionOfRoboSanta)
          keepingTracksOfSantaAndRoboSanta(
            nextDirections,
            currentPositionOfSanta,
            newPosition,
            visitedHouse + newPosition,
            true)
        }
      case List() =>
        visitedHouse
    }
  }
  
  val input = IO.getLines().mkString.toList
  val result = keepingTracksOfSantaAndRoboSanta(input).size
  println(s"Number of houses receiving at least a present by Santa and RoboSanta: ${result}")
}