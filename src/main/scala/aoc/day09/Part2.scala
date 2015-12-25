package aoc.day09

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

The next year, just to show off, Santa decides to take the route with the 
longest distance instead.

He can still start and end at any two (different) locations he wants, and 
he still must visit each location exactly once.

For example, given the distances above, the longest route would be 982 via 
(for example) Dublin -> London -> Belfast.

What is the distance of the longest route?
 */
  import Part1.{ Graph, linesToGraph }

  val input = IO.getLines()
  val g = linesToGraph(input)

  val maxPath = g.maxTour()
  println("Longest route:")
  println(s"$maxPath -> cost=${maxPath.cost()}")
}