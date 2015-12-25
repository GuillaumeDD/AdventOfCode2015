package aoc.day06

import io.IO

object Part1 extends App {
  /*
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year
after year, you've decided to deploy one million lights in a 1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed you instructions
on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner 
are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off,
 or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents 
 opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore 
 refers to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by doing the 
instructions Santa sent you in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones 
    that were on, and turning on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?
 */

  object SantaCommand extends Enumeration {
    type SantaCommand = Value
    val turn_on, turn_off, toggle = Value
  }

  import SantaCommand._
  type Point = (Int, Int)
  case class Command(action: SantaCommand, start: Point, end: Point)

  object Grid {
    private val grid = Array.ofDim[Boolean](1000, 1000)
    // Initialisation
    turn_off((0, 0), (999, 999))

    def nbLightsOn(): Int = {
      var sum = 0
      for {
        i <- Range(0, 1000)
        j <- Range(0, 1000)
        if grid(i)(j) // if the light is on
      } {
        sum += 1
      }
      sum
    }

    def apply(command: Command): Unit = {
      command match {
        case Command(SantaCommand.turn_on, start, end)  => this.turn_on(start, end)
        case Command(SantaCommand.turn_off, start, end) => this.turn_off(start, end)
        case Command(SantaCommand.toggle, start, end)   => this.toggle(start, end)
      }
    }

    def turn_on(start: Point, end: Point): Unit = {
      val (x1, y1) = start
      val (x2, y2) = end
      for {
        i <- Range(x1, x2).inclusive
        j <- Range(y1, y2).inclusive
      } {
        grid(i)(j) = true
      }
    }

    def turn_off(start: Point, end: Point): Unit = {
      val (x1, y1) = start
      val (x2, y2) = end
      for {
        i <- Range(x1, x2).inclusive
        j <- Range(y1, y2).inclusive
      } {
        grid(i)(j) = false
      }
    }

    def toggle(start: Point, end: Point): Unit = {
      val (x1, y1) = start
      val (x2, y2) = end
      for {
        i <- Range(x1, x2).inclusive
        j <- Range(y1, y2).inclusive
      } {
        grid(i)(j) = !grid(i)(j)
      }
    }
  }

  // Input
  def lineToCommand(line: String): Command = {
    val regex = """(toggle|turn off|turn on) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)""".r

    line match {
      case regex(command, x1, y1, x2, y2) =>
        val point1 = (x1.toInt, y1.toInt)
        val point2 = (x2.toInt, y2.toInt)
        command match {
          case "toggle"   => Command(toggle, point1, point2)
          case "turn off" => Command(turn_off, point1, point2)
          case "turn on"  => Command(turn_on, point1, point2)
        }
    }
  }
  val input = IO.getLines(lineToCommand)
  
  println(s"Initialisation of the grid, number of lights on: ${Grid.nbLightsOn}")
  input.foreach(Grid(_))
  println(s"Carrying out Santa's commands, number of lights on: ${Grid.nbLightsOn}")
}