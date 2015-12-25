package aoc.day05

import io.IO

object Part2 extends App {
  /*
--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of determining 
whether a string is naughty or nice. None of the old rules apply, as they are all 
clearly ridiculous.

Now, a nice string is one with all of the following properties:

    It contains a pair of any two letters that appears at least twice in the string without 
    overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
    It contains at least one letter which repeats with exactly one letter between them, 
    like xyx, abcdefeghi (efe), or even aaa.

For example:

    qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that
    repeats with exactly one letter between them (zxz).
    xxyxx is nice because it has a pair that appears twice and a letter that repeats with 
    one between, even though the letters used by each rule overlap.
    uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single 
    letter between them.
    ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but 
    no pair that appears twice.

How many strings are nice under these new rules?
 */

  def hasPair(str: String): Boolean = {
    val regex = """([a-z][a-z]).*\1""".r // Regex and back reference
    regex.findFirstIn(str).isDefined
  }

  def repeatingLetters(str: String): Boolean = {
    def isValid(l: List[Char]): Boolean =
      l(0) == l(2)
    str.toList.sliding(3).exists(isValid(_))
  }

  def isNice2(str: String): Boolean =
    hasPair(str) && repeatingLetters(str)

  val input = IO.getLines()
  println(s"Number of nice strings (new version): ${input.filter(isNice2(_)).size}")  
}