package aoc.day17

import io.IO

object Part1 extends App {
  /*
--- Day 17: No Such Thing as Too Much ---

The elves bought too much eggnog again - 150 liters this time. 
To fit it all into your refrigerator, you'll need to move it 
into smaller containers. You take an inventory of the capacities 
of the available containers.

For example, suppose you have containers of size 20, 15, 10, 5, 
and 5 liters. If you need to store 25 liters, there are four 
ways to do it:

    15 and 10
    20 and 5 (the first 5)
    20 and 5 (the second 5)
    15, 5, and 5

Filling all containers entirely, how many different combinations 
of containers can exactly fit all 150 liters of eggnog?
   */
  def combinationsWithRepetition[T](l: List[T], size: Int): List[List[T]] = {
    (size, l) match {
      case (0, _) =>
        // There is only one way to take 0 element from a list
        // It is to take the empty list.
        List(List())
      case (_, List()) =>
        // There is no way to take element from an empty list
        // (except by 0, dealt by the above case)
        List()
      case (s, head :: tail) =>
        combinationsWithRepetition(tail, s - 1).map(ll => head :: ll) ++
          combinationsWithRepetition(tail, s)
    }
  }

  val input = IO.getLines(_.toInt)
  var somme = 0
  val solutions = for {
    i <- 1 to input.length
    potentialSol <- combinationsWithRepetition(input, i)
    if potentialSol.sum == 150
  } {
    somme += 1
  }
  println(s"The number of different combinations of containers that can exactly fit all 150 liters of eggnog is: $somme")
}