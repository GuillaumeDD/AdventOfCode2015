package aoc.day24

import io.IO
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }

object Part1Parallel extends App {
  /*
--- Day 24: It Hangs in the Balance ---

It's Christmas Eve, and Santa is loading up the sleigh for this year's 
deliveries. However, there's one small problem: he can't get the sleigh 
to balance. If it isn't balanced, he can't defy physics, and nobody gets 
presents this year.

No pressure.

Santa has provided you a list of the weights of every package he needs to 
fit on the sleigh. The packages need to be split into three groups of exactly 
the same weight, and every package has to fit. The first group goes in the 
passenger compartment of the sleigh, and the second and third go in containers 
on either side. Only when all three groups weigh exactly the same amount will 
the sleigh be able to fly. Defying physics has rules, you know!

Of course, that's not the only problem. The first group - the one going in the 
passenger compartment - needs as few packages as possible so that Santa has some 
legroom left over. It doesn't matter how many packages are in either of the other 
two groups, so long as all of the groups weigh the same.

Furthermore, Santa tells you, if there are multiple ways to arrange the packages 
such that the fewest possible are in the first group, you need to choose the way 
where the first group has the smallest quantum entanglement to reduce the chance 
of any "complications". The quantum entanglement of a group of packages is the 
product of their weights, that is, the value you get when you multiply their 
weights together. Only consider quantum entanglement if the first group has the 
fewest possible number of packages in it and all groups weigh the same amount.

For example, suppose you have ten packages with weights 1 through 5 and 7 through 11. 
For this situation, the unique first groups, their quantum entanglements, and a 
way to divide the remaining packages are as follows:

Group 1;             Group 2; Group 3
11 9       (QE= 99); 10 8 2;  7 5 4 3 1
10 9 1     (QE= 90); 11 7 2;  8 5 4 3
10 8 2     (QE=160); 11 9;    7 5 4 3 1
10 7 3     (QE=210); 11 9;    8 5 4 2 1
10 5 4 1   (QE=200); 11 9;    8 7 3 2
10 5 3 2   (QE=300); 11 9;    8 7 4 1
10 4 3 2 1 (QE=240); 11 9;    8 7 5
9 8 3      (QE=216); 11 7 2;  10 5 4 1
9 7 4      (QE=252); 11 8 1;  10 5 3 2
9 5 4 2    (QE=360); 11 8 1;  10 7 3
8 7 5      (QE=280); 11 9;    10 4 3 2 1
8 5 4 3    (QE=480); 11 9;    10 7 2 1
7 5 4 3 1  (QE=420); 11 9;    10 8 2

Of these, although 10 9 1 has the smallest quantum entanglement (90), the configuration 
with only two packages, 11 9, in the passenger compartment gives Santa the most legroom 
and wins. In this situation, the quantum entanglement for the ideal configuration is 
therefore 99. Had there been two configurations with only two packages in the first group,
the one with the smaller quantum entanglement would be chosen.

What is the quantum entanglement of the first group of packages in the ideal configuration?
 */
  type Package = BigInt

  case class Group(content: List[Package]) {
    def size: Int = content.size
    def quantumEntanglement(): Package =
      content.product
  }

  def minQuantumEntanglement(
    l: List[Package],
    nbGroup: Int = 3): Package = {
    require(l.sum % nbGroup == 0,
      s"Cannot split evenly the package weights given the number of groups $nbGroup")
    val amountPerPackage = l.sum / nbGroup

    // Helper function
    def canBeSplit(lRest: List[Package], nbPart: Int): Boolean = {
      if (nbPart == 1) {
        // A unique package is correct if its sum is equal to the expected amount per package
        lRest.sum == amountPerPackage
      } else {
        // nbPart > 1
        val n = lRest.size
        var i = 1 // Size of the partition attempted
        var found = false // Flag telling whether or not we have found an adequate partition

        // Approach: try to find an appropriate split and stop ASAP
        while (i < n && !found) {
          val combinations = lRest.combinations(i) // Computation of combinations of size i
          // For each combination, try to split the rest of the list
          while (combinations.hasNext && !found) {
            val comb = combinations.next() // Get the first combination of size i (the first partition)
            val rest = lRest.diff(comb) // Computation of the second part of the partition
            found = canBeSplit(rest, nbPart - 1) // See if it can be adequately split
          }
          i += 1
        }
        found
      }
    }

    def checkCombination(
      c: List[Package],
      nbG: Int): Future[Option[Group]] =
      Future {
        if (c.sum == amountPerPackage) {
          val rest = l.diff(c)
          if (canBeSplit(rest, nbG)) {
            Some(Group(c))
          } else {
            None
          }
        } else {
          None
        }
      }

    val n = l.size
    var i = 1
    var found = false
    var resultGroup = List[Group]()
    while (i < n && !found) {
      println(s"\t -> trying with a first part of size $i")
      // Parallelization of the computation
      val futures = Future.sequence(l.combinations(i).map(checkCombination(_, nbGroup - 1)).toList)
      // Awaiting results
      val results = Await.result(futures, Duration.Inf)

      resultGroup =
        (for {
          resultingComputation <- results
          group <- resultingComputation
        } yield (group))

      found = resultGroup.size > 0
      i += 1
    }

    resultGroup.map(_.quantumEntanglement()).min
  }

  // Input
  def lineToPackage(line: String): Package = line.toInt
  val input = IO.getLines(lineToPackage)

  println(s"Computing...")

  val result = minQuantumEntanglement(input)
  println(s"The quantum entanglement of the first group of packages in the ideal configuration is: $result")
}