package aoc.day23

import scala.collection.mutable
import io.IO

object Part2 extends App {
  /*
  --- Part Two ---

The unknown benefactor is very thankful for releasi-- er, 
helping little Jane Marie with her computer. Definitely not 
to distract you, what is the value in register b after the 
program is finished executing if register a starts as 1 
instead?
 */
  import Part1.{ Program, lineToInstruction }

  val input = IO.getLines(lineToInstruction)
  val program2 = Program(input, mutable.Map("a" -> 1))

  program2.execute()
  val b = program2.register("b")
  println(s"b=$b")
}