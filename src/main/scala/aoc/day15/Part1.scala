package aoc.day15

import io.IO

object Part1 extends App {
  /*
--- Day 15: Science for Hungry People ---

Today, you set out on the task of perfecting your milk-dunking cookie recipe.
All you have to do is find the right balance of ingredients.

Your recipe leaves room for exactly 100 teaspoons of ingredients. 
You make a list of the remaining ingredients you could use to finish the 
recipe (your puzzle input) and their properties per teaspoon:

    capacity (how well it helps the cookie absorb milk)
    durability (how well it keeps the cookie intact when full of milk)
    flavor (how tasty it makes the cookie)
    texture (how it improves the feel of the cookie)
    calories (how many calories it adds to the cookie)

You can only measure ingredients in whole-teaspoon amounts accurately, and 
you have to be accurate so you can reproduce your results in the future. The 
total score of a cookie can be found by adding up each of the properties 
(negative totals become 0) and then multiplying together everything except 
calories.

For instance, suppose you have these two ingredients:

Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon 
(because the amounts of each ingredient must add up to 100) would result in a 
cookie with the following properties:

    A capacity of 44*-1 + 56*2 = 68
    A durability of 44*-2 + 56*3 = 80
    A flavor of 44*6 + 56*-2 = 152
    A texture of 44*3 + 56*-1 = 76

Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results
in a total score of 62842880, which happens to be the best score possible given 
these ingredients. If any properties had produced a negative total, it would have 
instead become zero, causing the whole score to multiply to zero.

Given the ingredients in your kitchen and their properties, what is the total 
score of the highest-scoring cookie you can make?
   */
  case class Ingredient(name: String,
                        capacity: Int,
                        durability: Int,
                        flavor: Int,
                        texture: Int,
                        calories: Int)

  // Input
  def lineToIngredient(line: String): Ingredient = {
    val regex =
      """([A-Za-z]+): capacity ([+-]{0,1}[0-9]+), durability ([+-]{0,1}[0-9]+), flavor ([+-]{0,1}[0-9]+), texture ([+-]{0,1}[0-9]+), calories ([+-]{0,1}[0-9]+)""".r
    line match {
      case regex(name, capacity, durability, flavor, texture, calories) =>
        Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    }
  }
  val ingredients = IO.getLines(lineToIngredient)
  require(ingredients.size == 4)

  val possibleCombinations = for {
    i <- 0 to 100
    j <- 0 to 100
    k <- 0 to 100
    l <- 0 to 100
    if i + j + k + l == 100
  } yield ((i, j, k, l))

  def score(distribution: (Int, Int, Int, Int)): Int = {
    def zeroed(s: Int): Int =
      Math.max(0, s)

    val (i, j, k, l) = distribution
    val capacity = zeroed(i * ingredients(0).capacity + j * ingredients(1).capacity +
      k * ingredients(2).capacity + l * ingredients(3).capacity)
    val durability = zeroed(i * ingredients(0).durability + j * ingredients(1).durability +
      k * ingredients(2).durability + l * ingredients(3).durability)
    val flavor = zeroed(i * ingredients(0).flavor + j * ingredients(1).flavor +
      k * ingredients(2).flavor + l * ingredients(3).flavor)
    val texture = zeroed(i * ingredients(0).texture + j * ingredients(1).texture +
      k * ingredients(2).texture + l * ingredients(3).texture)

    capacity * durability * flavor * texture
  }

  val maxScore = possibleCombinations.map(score(_)).max
  println(s"The total score of the highest-scoring cookie is: $maxScore")
  
  /*
--- Part Two ---

Your cookie recipe becomes wildly popular! Someone asks if you can make another 
recipe that has exactly 500 calories per cookie (so they can use it as a meal 
replacement). Keep the rest of your award-winning process the same (100 teaspoons, 
same ingredients, same scoring system).

For example, given the ingredients above, if you had instead selected 40 teaspoons
of butterscotch and 60 teaspoons of cinnamon (which still adds to 100), the total 
calorie count would be 40*8 + 60*3 = 500. The total score would go down, though: 
only 57600000, the best you can do in such trying circumstances.

Given the ingredients in your kitchen and their properties, what is the total score 
of the highest-scoring cookie you can make with a calorie total of 500?
 */

  def calories(distribution: (Int, Int, Int, Int)): Int = {
    val (i, j, k, l) = distribution
    i * ingredients(0).calories + j * ingredients(1).calories +
      k * ingredients(2).calories + l * ingredients(3).calories
  }

  val maxScore2 = possibleCombinations
    .filter(dist => calories(dist) == 500)
    .map(score(_)).max
  println(s"The total score of the highest-scoring cookie with a calorie total of 500 is: $maxScore2")
}