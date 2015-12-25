package aoc.day07

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

Now, take the signal you got on wire a, override wire b to that signal, 
and reset the other wires (including wire a). What new signal is ultimately
provided to wire a?
*/
  import Part1.{ WireBox, stringToAssignementExecution }
  
  val input = IO.getLines()
  for (op <- input) {
    stringToAssignementExecution(op)
  }
  val newOperation = "16076 -> b"
  stringToAssignementExecution(newOperation)
  println(s"New signal provided to a: ${WireBox("a")}")
}