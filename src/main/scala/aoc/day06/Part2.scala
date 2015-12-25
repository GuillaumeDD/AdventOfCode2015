package aoc.day06

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

You just finish implementing your winning light pattern when you realize you 
mistranslated Santa's message from Ancient Nordic Elvish.

The light grid you bought actually has individual brightness controls; each light 
can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those 
lights by 1.

The phrase turn off actually means that you should decrease the brightness of those 
lights by 1, to a minimum of zero.

The phrase toggle actually means that you should increase the brightness of those 
lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:

    turn on 0,0 through 0,0 would increase the total brightness by 1.
    toggle 0,0 through 999,999 would increase the total brightness by 2000000.
 */
  import Part1.{ Command, SantaCommand, Point, lineToCommand }
  object BrightnessGrid {
    private val grid = Array.ofDim[Int](1000, 1000)
    // Initialisation
    init()

    def totalBrightness(): Int = {
      grid.map(_.sum).sum
    }

    def apply(command: Command): Unit = {
      command match {
        case Command(SantaCommand.turn_on, start, end)  => this.turn_on(start, end)
        case Command(SantaCommand.turn_off, start, end) => this.turn_off(start, end)
        case Command(SantaCommand.toggle, start, end)   => this.toggle(start, end)
      }
    }

    def init(): Unit = {
      for {
        i <- Range(0, 1000)
        j <- Range(0, 1000)
      } {
        grid(i)(j) = 0
      }
    }

    def turn_on(start: Point, end: Point): Unit = {
      val (x1, y1) = start
      val (x2, y2) = end
      for {
        i <- Range(x1, x2).inclusive
        j <- Range(y1, y2).inclusive
      } {
        grid(i)(j) += 1
      }
    }

    def turn_off(start: Point, end: Point): Unit = {
      val (x1, y1) = start
      val (x2, y2) = end
      for {
        i <- Range(x1, x2).inclusive
        j <- Range(y1, y2).inclusive
      } {
        if (grid(i)(j) > 0) { // each light can have a brightness of zero or more
          grid(i)(j) -= 1
        }
      }
    }

    def toggle(start: Point, end: Point): Unit = {
      val (x1, y1) = start
      val (x2, y2) = end
      for {
        i <- Range(x1, x2).inclusive
        j <- Range(y1, y2).inclusive
      } {
        grid(i)(j) += 2
      }
    }
  }

  val input = IO.getLines(lineToCommand)

  println(s"Initialisation of the brightness grid, total brightness: ${BrightnessGrid.totalBrightness}")
  input.foreach(BrightnessGrid(_))
  println(s"Carrying out Santa's commands, total brightness: ${BrightnessGrid.totalBrightness}")
}