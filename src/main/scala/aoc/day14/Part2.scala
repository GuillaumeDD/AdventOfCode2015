package aoc.day14

import io.IO
import scala.collection.mutable

object Part2 extends App {
  /*
--- Part Two ---

Seeing how reindeer move in bursts, Santa decides he's not pleased 
with the old scoring system.

Instead, at the end of each second, he awards one point to the reindeer
currently in the lead. (If there are multiple reindeer tied for the lead, 
they each get one point.) He keeps the traditional 2503 second time limit,
of course, as doing otherwise would be entirely ridiculous.

Given the example reindeer from above, after the first second, Dancer 
is in the lead and gets one point. He stays in the lead until several 
seconds into Comet's second burst: after the 140th second, Comet pulls 
into the lead and gets his first point. Of course, since Dancer had 
been in the lead for the 139 seconds before that, he has accumulated 
139 points by the 140th second.

After the 1000th second, Dancer has accumulated 689 points, while poor 
Comet, our old champion, only has 312. So, with the new scoring system, 
Dancer would win (if the race ended at 1000 seconds).

Again given the descriptions of each reindeer (in your puzzle input), 
after exactly 2503 seconds, how many points does the winning reindeer have?
   */
  import Part1.{ linesToReinders, distance }
  val reinders = linesToReinders(IO.getLines())
  val reinderNames = reinders.map(_.name)
  // Initialization of the scoreboard
  val points = mutable.Map[String, Int]()
  for (name <- reinderNames) {
    points(name) = 0
  }

  for (tic <- 1 to 2503) {
    val distances = reinders.map(r => (r, distance(tic, r)))
    val (rMax, max) = distances.maxBy(_._2)

    for {
      (r, d) <- distances
      if d == max
    } {
      points(r.name) = points(r.name) + 1
    }
  }

  println(s"After exactly 2503 seconds, the winning reindeer have ${points.maxBy(_._2)} points")
}