package aoc.day13

import io.IO

object Part1 extends App {
  /*
--- Day 13: Knights of the Dinner Table ---

In years past, the holiday feast with your family hasn't gone so well. 
Not everyone gets along! This year, you resolve, will be different. 
You're going to find the optimal seating arrangement and avoid all those 
awkward conversations.

You start by writing up a list of everyone invited and the amount their 
happiness would increase or decrease if they were to find themselves 
sitting next to each other person. You have a circular table that will 
be just big enough to fit everyone comfortably, and so each person will 
have exactly two neighbors.

For example, suppose you have only four attendees planned, and you calculate
their potential happiness as follows:

Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.

Then, if you seat Alice next to David, Alice would lose 2 happiness units 
(because David talks so much), but David would gain 46 happiness units 
(because Alice is such a good listener), for a total change of 44.

If you continue around the table, you could then seat Bob next to Alice 
(Bob gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob 
(Carol gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). 
The arrangement looks like this:

     +41 +46
+55   David    -2
Carol       Alice
+60    Bob    +54
     -7  +83

After trying every other seating arrangement in this hypothetical scenario, 
you find that this one is the most optimal, with a total change in happiness
of 330.

What is the total change in happiness for the optimal seating arrangement 
of the actual guest list?
   */
  case class Node(name: String)
  case class Edge(from: Node, to: Node, cost: Int)

  case class Graph(nodes: Set[Node], edges: Set[Edge]) {
    def next(n: Node): Set[Node] =
      edges.filter { edge => edge.from == n }.map(_.to)

    def edge(from: Node, to: Node): Edge =
      edges.filter { edge => edge.from == from }
        .filter(_.to == to).head

    case class Path(p: List[Node]) {
      def cost(): Int =
        p.sliding(2).map {
          case List(from, to) => edge(from, to)
        }.map(_.cost).sum

      def costForwardAndBackward(): Int =
        cost() +
          Path(p.reverse).cost()

      def toCycle(): Path =
        if (p.size > 1) {
          Path(p :+ p.head)
        } else {
          this
        }

      override def toString =
        p.mkString(" -> ")
    }

    def tour(): Set[Path] = {
      def computePath(currentPath: List[Node]): Set[Path] = {
        require(currentPath.nonEmpty, "Currrent path is not empty")
        // Take the current node
        val currentNode = currentPath.head

        // Get the next possible nodes
        val nextNodes = next(currentNode)
          // Filter already visited nodes
          .filterNot(currentPath.contains(_))
        //println(s"nextNodes of ${currentNode}: ${nextNodes}")

        // Computation of next path
        if (nextNodes.isEmpty) {
          // Impossible to go further
          Set(Path(currentPath.reverse))
        } else {
          // Computation of next paths
          nextNodes.flatMap {
            nextNode => computePath(nextNode :: currentPath)
          }
        }

      }

      nodes.flatMap {
        startNode => computePath(List(startNode))
      }

    }

    def cycles(): Set[Path] =
      tour().map(_.toCycle())

    def minTour(): Path =
      tour().minBy(_.cost)

    def maxTour(): Path =
      tour().maxBy(_.cost)

    def minTourForwardAndBackward(): Path =
      tour().minBy(_.costForwardAndBackward())

    def maxTourForwardAndBackward(): Path =
      tour().maxBy(_.costForwardAndBackward())

    def minCycle(): Path =
      cycles().minBy(_.cost)

    def maxCycle(): Path =
      cycles().maxBy(_.cost)

    def minCycleForwardAndBackward(): Path =
      cycles().minBy(_.costForwardAndBackward())

    def maxCycleForwardAndBackward(): Path =
      cycles().maxBy(_.costForwardAndBackward())
  }

  // Input
  def linesToGraph(lines: List[String]): Graph = {
    val regex = """([a-zA-Z]+) would (lose|gain) ([0-9]+) happiness units by sitting next to ([a-zA-Z]+).""".r

    def translateLine(line: String): (Node, Node, Int) =
      line match {
        case regex(from, operation, cost, to) =>
          val c = operation match {
            case "lose" => -cost.toInt
            case "gain" => cost.toInt
          }
          (Node(from), Node(to), c)
      }

    var nodes = Set[Node]()
    var edges = Set[Edge]()

    for {
      line <- lines
    } {
      val (from, to, cost) = translateLine(line)
      nodes += from
      nodes += to
      edges += Edge(from, to, cost)
    }

    Graph(nodes, edges)
  }

  val g = linesToGraph(IO.getLines())
  val path = g.maxCycleForwardAndBackward()
  println("The total change in happiness for the optimal seating arrangement " +
    s"of the actual guest list is:\n$path -> cost=${path.costForwardAndBackward()}")
}