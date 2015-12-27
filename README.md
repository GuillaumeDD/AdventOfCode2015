# Description #
This project provides solutions to the [Advent of Code 2015](http://adventofcode.com).

# Installation #
You can run the solutions via [SBT](http://www.scala-sbt.org/).

First, compile the solutions:

	$ sbt compile

Then, you can  run the solution to a specific  problem. For instance, to
run the solution for the part 1 of day 1:

	$ sbt "runMain aoc.day01.Part1" < src/main/resources/day01/input
	
To run the solution for the part 2 of day 1:

	$ sbt "runMain aoc.day01.Part2" < src/main/resources/day01/input

# Notes about the Provided Solutions #
1. Day 1: Not Quite Lisp
1. Day 2: I Was Told There Would Be No Math
1. Day 3: Perfectly Spherical Houses in a Vacuum
1. Day 4: The Ideal Stocking Stuffer
1. Day 5: Doesn't He Have Intern-Elves For This?
   + Usage of regular expressions and backreferences
1. Day 6: Probably a Fire Hazard
1. Day 7: Some Assembly Required
1. Day 8: Matchsticks
1. Day 9: All in a Single Night
   + Brute force approach
1. Day 10: Elves Look, Elves Say
1. Day 11: Corporate Policy
   + Usage of regular expressions and backreferences
1. Day 12: JSAbacusFramework.io
   + Usage of spray.json library
1. Day 13: Knights of the Dinner Table
   + Similar approach to day 9
1. Day 14: Reindeer Olympics
1. Day 15: Science for Hungry People
1. Day 16: Aunt Sue
1. Day 17: No Such Thing as Too Much
1. Day 18: Like a GIF For Your Yard
1. Day 19: Medicine for Rudolph
   + Part 2 implements a solution using an A* algorithm
1. Day 20: Infinite Elves and Infinite Houses
   + Solutions runs slowly.
1. Day 21: RPG Simulator 20XX
1. Day 22: Wizard Simulator 20XX
1. Day 23: Opening the Turing Lock
1. Day 24: It Hangs in the Balance
   + This solution proceeds by building a first group by ascending
     size and by ascending quantum entanglement. Then it checks that
     the other groups can be built so that all groups have the same
     weight.
   + It takes a few seconds to compute the solution
1. Day 25: Let It Snow 

# License #
GPLv3 - see the COPYING file.
