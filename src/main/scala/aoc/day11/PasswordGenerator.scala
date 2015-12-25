package aoc.day11

import scala.annotation.tailrec

object PasswordGenerator {
  val following = "abcdefghijklmnopqrstuvwxyz".sliding(3).toList
  val regex = """(.)\1.*(.)\2""".r // Regex and back reference

  def isValidPassword(s: String): Boolean = {
    def contains3followingChar(s: String): Boolean = {
      s.sliding(3).exists { x => following.contains(x) }
    }

    def containsIOL(s: String): Boolean =
      s.contains('i') || s.contains('l') || s.contains('l')

    def has2Pairs(str: String): Boolean = {
      regex.findFirstIn(str).isDefined
    }

    contains3followingChar(s) && !containsIOL(s) && has2Pairs(s)
  }

  @tailrec
  def increment(password: String): String = {
    def incrementHelper(s: List[Char]): List[Char] =
      s match {
        case 'z' :: rest =>
          'a' :: incrementHelper(rest)
        case other :: rest =>
          (other + 1).toChar :: rest
        case List() =>
          List()
      }

    val newPassword = incrementHelper(password.reverse.toList).reverse.mkString
    if (isValidPassword(newPassword)) {
      newPassword
    } else {
      increment(newPassword)
    }
  }
}