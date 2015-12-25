package aoc.day18
import io.IO

object Part1 extends App {
  /*
--- Day 18: Like a GIF For Your Yard ---

After the million lights incident, the fire code has gotten stricter: 
now, at most ten thousand lights are allowed. You arrange them in a 
100x100 grid.

Never one to let you down, Santa again mails you instructions on the 
ideal lighting configuration. With so few lights, he says, you'll have 
to resort to animation.

Start by setting your lights to the included initial configuration 
(your puzzle input). A # means "on", and a . means "off".

Then, animate your grid in steps, where each step decides the next 
configuration based on the current one. Each light's next state 
(either on or off) depends on its current state and the current states 
of the eight lights adjacent to it (including diagonals). Lights on 
the edge of the grid might have fewer than eight neighbors; the missing 
ones always count as "off".

For example, in a simplified 6x6 grid, the light marked A has the 
neighbors numbered 1 through 8, and the light marked B, which is on 
an edge, only has the neighbors marked 1 through 5:

1B5...
234...
......
..123.
..8A4.
..765.

The state a light should have next is based on its current state 
(on or off) plus the number of neighbors that are on:

    A light which is on stays on when 2 or 3 neighbors are on, and 
    turns off otherwise.
    A light which is off turns on if exactly 3 neighbors are on, and 
    stays off otherwise.

All of the lights update simultaneously; they all consider the same 
current state before moving to the next.

Here's a few steps from an example configuration of another 6x6 grid:

Initial state:
.#.#.#
...##.
#....#
..#...
#.#..#
####..

After 1 step:
..##..
..##.#
...##.
......
#.....
#.##..

After 2 steps:
..###.
......
..###.
......
.#....
.#....

After 3 steps:
...#..
......
...#..
..##..
......
......

After 4 steps:
......
......
..##..
..##..
......
......

After 4 steps, this example has four lights on.

In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps?
   */

  object Light extends Enumeration {
    val ON, OFF = Value
    type Light = Value

    def toString(l: Light): String =
      l match {
        case ON  => "#"
        case OFF => "."
      }
  }
  import Light._
  case class Grid(content: Array[Array[Light]]) {
    def width(): Int =
      content(0).length

    def length(): Int =
      content.length

    def status(p: (Int, Int)): Light = {
      require(inGrid(p), s"Point $p is not in the grid")
      content(p._1)(p._2)
    }

    def inGrid(p: (Int, Int)): Boolean = {
      val (x, y) = p
      x >= 0 && y >= 0 && x < width() && y < length()
    }

    def neighbors(p: (Int, Int)): Set[(Int, Int)] = {
      val (i, j) = p
      Set((i - 1, j - 1), (i - 1, j), (i - 1, j + 1), (i, j - 1), (i, j + 1),
        (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)).filter(inGrid(_))
    }

    def nbLightON(): Int = {
      var somme = 0
      for {
        i <- Range(0, width)
        j <- Range(0, length)
        if status(i, j) == Light.ON
      } {
        somme += 1
      }
      somme
    }

    override def toString =
      (for (line <- content)
        yield (line.map(Light.toString(_)).mkString))
        .mkString("\n")
  }

  case class CellularAutomata(
      grid: Grid,
      transition: Grid => Grid) {

    def step(): CellularAutomata =
      CellularAutomata(transition(grid), transition)

    def nbLightON(): Int = grid.nbLightON()

    override def toString =
      grid.toString
  }

  def transition(g: Grid): Grid = {
    val newGrid = Array.ofDim[Light](g.width, g.length)
    for {
      i <- Range(0, g.width)
      j <- Range(0, g.length)
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

  // Input
  def inputToCellularAutomata(str: List[String]): Grid = {
    def toLight(s: Char): Light =
      s match {
        case '.' => Light.OFF
        case '#' => Light.ON
      }

    val lines =
      for (line <- str)
        yield (line.map(toLight(_)).toArray)

    Grid(lines.toArray)
  }
  val input = inputToCellularAutomata(IO.getLines())
  var automata = CellularAutomata(input, transition)
  for (_ <- 1 to 100) {
    automata = automata.step()
  }

  println(s"The number of lights ON on after 100 steps is: ${automata.nbLightON()}")
}