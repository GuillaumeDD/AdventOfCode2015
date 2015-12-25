package aoc.day13

import io.IO

object Part2 extends App {
  /*  
  --- Part Two ---

In all the commotion, you realize that you forgot to seat yourself.
At this point, you're pretty apathetic toward the whole thing, and 
your happiness wouldn't really go up or down regardless of who you 
sit next to. You assume everyone else would be just as ambivalent 
about sitting next to you, too.

So, add yourself to the list, and give all happiness relationships 
that involve you a score of 0.

What is the total change in happiness for the optimal seating 
arrangement that actually includes yourself?
*/
  import Part1.{ linesToGraph, Graph, Node, Edge }
  val g = linesToGraph(IO.getLines())
  val path = g.maxCycleForwardAndBackward()
  val me = Node("me")
  val nodes = g.nodes + me
  val edges = g.edges
  var newEdges = Set[Edge]()
  for (n <- g.nodes) {
    newEdges += Edge(n, me, 0)
    newEdges += Edge(me, n, 0)
  }

  val g2 = Graph(nodes, edges ++ newEdges)
  val path2 = g2.maxCycleForwardAndBackward()
  println("The total change in happiness for the optimal seating " +
    s"arrangement that actually includes yourself:\n$path2 -> cost=${path2.costForwardAndBackward()}")
}