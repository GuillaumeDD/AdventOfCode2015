package aoc.day02

import io.IO

object Part1 extends App {
  /*
--- Day 2: I Was Told There Would Be No Math ---

The elves are running low on wrapping paper, and so they need to submit an order for more. 
They have a list of the dimensions (length l, width w, and height h) of each present, and 
only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which makes calculating 
the required wrapping paper for each gift a little easier: find the surface area of the box, which
is 2*l*w + 2*w*h + 2*h*l. The elves also need a little extra paper for each present: the area of 
the smallest side.

For example:

    A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper 
    plus 6 square feet of slack, for a total of 58 square feet.
    A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper
    plus 1 square foot of slack, for a total of 43 square feet.

All numbers in the elves' list are in feet. How many total square feet of wrapping paper should 
they order?
 */
  /*
   * Input of the right rectangular prism: (l, w, h)
   * Surface: 2*l*w + 2*w*h + 2*h*l
   */
  type Box = (Int, Int, Int)

  def surface(b: Box): Int = {
    val (l, w, h) = b
    2 * l * w + 2 * w * h + 2 * h * l
  }

  def smallestSideSurface(b: Box): Int = {
    val (l, w, h) = b
    val side1 = l * w
    val side2 = l * h
    val side3 = w * h
    Math.min(Math.min(side1, side2), side3)
  }

  def surfaceOfWrappingPaper(boxes: List[Box]): Int =
    boxes.map(b => surface(b) + smallestSideSurface(b)).sum

  def lineToTriple(line: String): (Int, Int, Int) = {
    val RectangularSize = """([0-9]+)x([0-9]+)x([0-9]+)""".r
    line match {
      case RectangularSize(l, w, h) => (l.toInt, w.toInt, h.toInt)
    }
  }

  val input = IO.getLines(lineToTriple)

  println(s"Surface of wrapping paper to order: ${surfaceOfWrappingPaper(input)}")
}