package aoc.day09

import io.IO

object Part1 extends App {
/*
--- Day 9: All in a Single Night ---

Every year, Santa manages to deliver all of his presents in a single night.

This year, however, he has some new locations to visit; his elves have 
provided him the distances between every pair of locations. He can start
and end at any two (different) locations he wants, but he must visit each
location exactly once. What is the shortest distance he can travel to 
achieve this?

For example, given the following distances:

London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141

The possible routes are therefore:

Dublin -> London -> Belfast = 982
London -> Dublin -> Belfast = 605
London -> Belfast -> Dublin = 659
Dublin -> Belfast -> London = 659
Belfast -> Dublin -> London = 605
Belfast -> London -> Dublin = 982

The shortest of these is London -> Dublin -> Belfast = 605, and so the answer
is 605 in this example.

What is the distance of the shortest route?  
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

    def minTour(): Path =
      tour().minBy(_.cost)

    def maxTour(): Path =
      tour().maxBy(_.cost)
  }
  
  // Input
  def linesToGraph(lines: List[String]): Graph = {
    val regex = """([a-zA-Z]+) to ([a-zA-Z]+) = ([0-9]+)""".r
    def translateLine(line: String): (Node, Node, Int) =
      line match {
        case regex(from, to, cost) =>
          (Node(from), Node(to), cost.toInt)
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
      edges += Edge(to, from, cost)
    }

    Graph(nodes, edges)
  }
  val input = IO.getLines()
  val g = linesToGraph(input)
  val path = g.minTour()
  println("Shortest route:")
  println(s"$path -> cost=${path.cost()}")
}