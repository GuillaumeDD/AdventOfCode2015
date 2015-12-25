package aoc.day05

import io.IO

object Part1 extends App {
  /*
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or nice.

A nice string is one with all of the following properties:

    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or 
    aabbccdd (aa, bb, cc, or dd).
    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the
    other requirements.

For example:

    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double 
    letter (...dd...), and none of the disallowed substrings.
    aaa is nice because it has at least three vowels and a double letter, even though the 
    letters used by different rules overlap.
    jchzalrnumimnmhp is naughty because it has no double letter.
    haegwjzuvuyypxyu is naughty because it contains the string xy.
    dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?
 */

  def numberOfVowels(str: String): Int = {
    val vowels = List('a', 'e', 'i', 'o', 'u')
    str.toList.filter(vowels.contains(_)).size
  }

  def containsTwiceLetters(str: String): Boolean =
    //val regex = """(.)\1""".r // May be using a regex with backreference
    str.toList.sliding(2).map(_.distinct).exists(_.size == 1)

  def doesNotContainsForbiddenString(str: String): Boolean =
    !str.contains("ab") && !str.contains("cd") && !str.contains("pq") && !str.contains("xy")

  def isNice(str: String): Boolean =
    numberOfVowels(str) >= 3 && containsTwiceLetters(str) && doesNotContainsForbiddenString(str)

  val input = IO.getLines()
  println(s"Number of nice strings: ${input.filter(isNice(_)).size}")
}