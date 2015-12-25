package aoc.day19

import scala.collection.mutable
import io.IO

object Part2 extends App {
  /*
--- Part Two ---

Now that the machine is calibrated, you're ready to begin molecule fabrication.

Molecule fabrication always begins with just a single electron, e, and applying 
replacements one at a time, just like the ones during calibration.

For example, suppose you have the following replacements:

e => H
e => O
H => HO
H => OH
O => HH

If you'd like to make HOH, you start with e, and then make the following 
replacements:

    e => O to get O
    O => HH to get HH
    H => OH (on the second H) to get HOH

So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can 
be made in 6 steps.

How long will it take to make the medicine? Given the available replacements and 
the medicine molecule in your puzzle input, what is the fewest number of steps to 
go from e to the medicine molecule?
 */
  import Part1.{ Replacement, Molecule, inputToReplacements, computeMolecules }

  def aStar(replacements: List[Replacement],
            start: Molecule,
            objectiveMolecule: Molecule,
            heuristics: Ordering[Molecule] = Ordering.by[Molecule, Int](_.size)): List[Molecule] = {
    // Frontier management
    val frontier = mutable.PriorityQueue.empty[Molecule](heuristics)

    frontier.enqueue(start)

    // Path management
    val cameFrom = mutable.Map[Molecule, Molecule]()
    cameFrom(start) = ""
    def buildPath(): List[Molecule] = {
      var path = List[Molecule](objectiveMolecule)
      var current = objectiveMolecule
      while (current != start) {
        current = cameFrom(current)
        path ::= current
      }
      path
    }

    // Algorithm
    var found = false
    while (!found && !frontier.isEmpty) {
      val currentMolecule = frontier.dequeue()

      if (currentMolecule == objectiveMolecule) {
        // Early exit
        found = true
      } else {
        for {
          m <- computeMolecules(replacements.toList, currentMolecule)
          if !cameFrom.contains(m) // Filter already visited molecules
        } {
          frontier.enqueue(m)
          cameFrom(m) = currentMolecule
        }
      }
    }

    buildPath()
  }

  val input = IO.getLines()
  val replacements = inputToReplacements(input.init.init)
  val molecule = input.last

  // From the given molecule to "e"
  val process = aStar(replacements.map(_.reverse),
    start = molecule,
    objectiveMolecule = "e",
    heuristics = Ordering.by[Molecule, Int](_.size).reverse)
  println(s"The fewest number of steps to go from e to the medicine molecule is: ${process.size} (number of replacements: ${process.size - 1})")
}