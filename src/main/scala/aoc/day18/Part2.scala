package aoc.day18

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

You flip the instructions over; Santa goes on to point out that this 
is all just an implementation of Conway's Game of Life. At least, it 
was, until you notice that something's wrong with the grid of lights
you bought: four lights, one in each corner, are stuck on and can't 
be turned off. The example above will actually run like this:

Initial state:
##.#.#
...##.
#....#
..#...
#.#..#
####.#

After 1 step:
#.##.#
####.#
...##.
......
#...#.
#.####

After 2 steps:
#..#.#
#....#
.#.##.
...##.
.#..##
##.###

After 3 steps:
#...##
####.#
..##.#
......
##....
####.#

After 4 steps:
#.####
#....#
...#..
.##...
#.....
#.#..#

After 5 steps:
##.###
.##..#
.##...
.##...
#.#...
##...#

After 5 steps, this example now has 17 lights on.

In your grid of 100x100 lights, given your initial configuration, but with 
the four corners always in the on state, how many lights are on after 100 steps?

 */
  import Part1.{ Grid, CellularAutomata, inputToCellularAutomata }
  import Part1.Light._

  def transition2(g: Grid): Grid = {
    val newGrid = Array.ofDim[Light](g.width, g.length)

    // Static corner
    val corners = List((0, 0), (0, 99), (99, 0), (99, 99))
    for ((x, y) <- corners) {
      newGrid(x)(y) = g.status((x, y))
    }

    // Dynamic change
    for {
      i <- Range(0, g.width)
      j <- Range(0, g.length)
      // Avoid corner 
      if !corners.contains((i, j))
    } {
      if (g.status(i, j) == ON) {
        // A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
        val numbersOfONNeighbors = g.neighbors(i, j).toList.map(g.status(_)).filter { s => s == ON }.size
        if ((numbersOfONNeighbors == 2) || (numbersOfONNeighbors == 3)) {
          newGrid(i)(j) = ON
        } else {
          newGrid(i)(j) = OFF
        }
      } else {
        // A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
        val numbersOfONNeighbors = g.neighbors(i, j).toList.map(g.status(_)).filter { s => s == ON }.size
        if ((numbersOfONNeighbors == 3)) {
          newGrid(i)(j) = ON
        } else {
          newGrid(i)(j) = OFF
        }
      }
    }

    Grid(newGrid)
  }

  val input = inputToCellularAutomata(IO.getLines())
  var automata = CellularAutomata(input, transition2)

  for (_ <- 1 to 100) {
    automata = automata.step()
  }
  println(s"The number of lights ON on after 100 steps is: ${automata.nbLightON()}")
}