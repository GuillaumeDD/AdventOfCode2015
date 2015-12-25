package aoc.day16

import io.IO

object Part2 extends App {
  /*
 --- Part Two ---

As you're about to send the thank you note, something in the MFCSAM's 
instructions catches your eye. Apparently, it has an outdated 
retroencabulator, and so the output from the machine isn't exact values 
- some of them indicate ranges.

In particular, the cats and trees readings indicates that there are greater
than that many (due to the unpredictable nuclear decay of cat dander and 
tree pollen), while the pomeranians and goldfish readings indicate that 
there are fewer than that many (due to the modial interaction of 
magnetoreluctance).

What is the number of the real Aunt Sue?
  */
  import Part1.{ Aunt, strToInfo }

  def matchingAunt2(a: Aunt): Boolean = {
    def check(auntInfo: Map[String, Int],
              checkValues: Map[String, Int]): Boolean = {
      // Helper function
      def contains(pair: (String, Int)): Boolean = {
        val (s, n) = pair
        if (auntInfo.contains(s)) {
          s match {
            case "cats" | "trees" =>
              auntInfo(s) > n
            case "pomeranians" | "goldfish" =>
              auntInfo(s) < n
            case other =>
              auntInfo(s) == n
          }
        } else {
          // Unavailable info
          true
        }
      }

      checkValues.forall(contains(_))
    }

    val checkValues = Map("children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3,
      "akitas" -> 0, "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3,
      "cars" -> 2, "perfumes" -> 1)
    check(a.info, checkValues)

  }

  val input = IO.getLines(strToInfo)
  val aunts2 = input.filter { matchingAunt2(_) }
  println(s"The number of the real Aunt Sue is: ${aunts2(0).n}")
}