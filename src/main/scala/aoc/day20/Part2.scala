package aoc.day20

import io.IO
import scala.collection.mutable

object Part2 extends App {
  /*
--- Part Two ---

The Elves decide they don't want to visit an infinite number of houses. 
Instead, each Elf will stop after delivering presents to 50 houses. To 
make up for it, they decide to deliver presents equal to eleven times 
their number at each house.

With these changes, what is the new lowest house number of the house to 
get at least as many presents as the number in your puzzle input?
   */

  import Part1.findMin

  def computeHouseNonInfinite(max: Int): Int = {
    val house = mutable.Map[Int, Int]()
    for {
      // For every Elf
      iElf <- 1 to max / 11
      // Computation of the number of delivered houses
      maxHouse = iElf * 50
      jHouse <- iElf to Math.min(max / 11, maxHouse) by iElf
    } {
      if (house.contains(jHouse)) {
        house(jHouse) = house(jHouse) + iElf * 11
      } else {
        house(jHouse) = iElf * 11
      }
    }

    findMin(house, max, 11)
  }

  val input = IO.getLines(_.toInt).head

  val result2 = computeHouseNonInfinite(input)
  println(s"The new lowest house number of the house to get at least as many presents as $input is: $result2")
}