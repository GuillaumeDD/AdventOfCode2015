package aoc.day14

import io.IO

object Part1 extends App {
  /*
--- Day 14: Reindeer Olympics ---

This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must 
rest occasionally to recover their energy. Santa would like to know which of 
his reindeer is fastest, and so he has them race.

Reindeer can only either be flying (always at their top speed) or resting 
(not moving at all), and always spend whole seconds in either state.

For example, suppose you have the following Reindeer:

    Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
    Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.

After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten 
seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh 
second, Comet begins resting (staying at 140 km), and Dancer continues on for
a total distance of 176 km. On the 12th second, both reindeer are resting. They
continue to rest until the 138th second, when Comet flies for another ten 
seconds. On the 174th second, Dancer flies for another 11 seconds.

In this example, after the 1000th second, both reindeer are resting, and Comet 
is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point).
So, in this situation, Comet would win (if the race ended at 1000 seconds).

Given the descriptions of each reindeer (in your puzzle input), after exactly 
2503 seconds, what distance has the winning reindeer traveled?
 */
  case class Reinder(name: String, speed: Int, flyingPeriod: Int, restingPeriod: Int)
  def distance(time: Int, r: Reinder): Int = {
    // Number of cycles (flyingPeriod + restingPeriod)
    val cycleTimes = time / (r.flyingPeriod + r.restingPeriod)
    // Number of seconds of flight in the started period
    val rest = Math.min(time % (r.flyingPeriod + r.restingPeriod), r.flyingPeriod)
    // Traveled distance
    (cycleTimes * r.flyingPeriod + rest) * r.speed
  }

  // Input
  def linesToReinders(lines: List[String]): Set[Reinder] = {
    val regex = """([a-zA-Z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.""".r

    def lineToReinder(line: String): Reinder =
      line match {
        case regex(name, speed, flyingPeriod, restingPeriod) =>
          Reinder(name, speed.toInt, flyingPeriod.toInt, restingPeriod.toInt)
      }

    lines.map(lineToReinder(_)).toSet
  }
  val reinders = linesToReinders(IO.getLines())
  val winner = reinders.maxBy(r => distance(2503, r))
  println(s"After exactly 2503 seconds, the winning reindeer has traveled:\n${winner}, ${distance(2503, winner)}")
}