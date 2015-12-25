package aoc.day17

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

While playing with all the containers in the kitchen, another 
load of eggnog arrives! The shipping and receiving department 
is requesting as many containers as you can spare.

Find the minimum number of containers that can exactly fit all 
150 liters of eggnog. How many different ways can you fill that 
number of containers and still hold exactly 150 litres?

In the example above, the minimum number of containers was two. 
There were three ways to use that many containers, and so the 
answer there would be 3.
 */
  import Part1.combinationsWithRepetition

  val input = IO.getLines(_.toInt)
  var somme = 0
  for {
    i <- 1 to input.length
  } {
    val potentialSol = combinationsWithRepetition(input, i).filter(_.sum == 150).size
    if (potentialSol > 0 && somme == 0) {
      somme = potentialSol
    }
  }
  println(s"The number of different combinations of containers is: $somme")
}