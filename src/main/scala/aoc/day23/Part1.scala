package aoc.day23

import io.IO
import scala.collection.mutable

object Part1 extends App {
  /*
--- Day 23: Opening the Turing Lock ---

Little Jane Marie just got her very first computer for Christmas 
from some unknown benefactor. It comes with instructions and an
example program, but the computer itself seems to be malfunctioning. 
She's curious what the program does, and would like you to help 
her run it.

The manual explains that the computer supports two registers and 
six instructions (truly, it goes on to remind the reader, a 
state-of-the-art technology). The registers are named a and b, can 
hold any non-negative integer, and begin with a value of 0. The 
instructions are as follows:

    hlf r sets register r to half its current value, then continues 
    with the next instruction.
    tpl r sets register r to triple its current value, then continues 
    with the next instruction.
    inc r increments register r, adding 1 to it, then continues with 
    the next instruction.
    jmp offset is a jump; it continues with the instruction offset 
    away relative to itself.
    jie r, offset is like jmp, but only jumps if register r is 
    even ("jump if even").
    jio r, offset is like jmp, but only jumps if register r is 1 
    ("jump if one", not odd).

All three jump instructions work with an offset relative to that 
instruction. The offset is always written with a prefix + or - to 
indicate the direction of the jump (forward or backward, respectively). 
For example, jmp +1 would simply continue with the next instruction, 
while jmp +0 would continuously jump back to itself forever.

The program exits when it tries to run an instruction beyond the 
ones defined.

For example, this program sets a to 2, because the jio instruction 
causes it to skip the tpl instruction:

inc a
jio a, +2
tpl a
inc a

What is the value in register b when the program in your puzzle input 
is finished executing?
   */

  type Register = String

  sealed abstract class Instruction
  case class Half(r: Register) extends Instruction
  case class Triple(r: Register) extends Instruction
  case class Increment(r: Register) extends Instruction
  case class Jump(offset: Int) extends Instruction
  case class JumpIfEven(r: Register, offset: Int) extends Instruction
  case class JumpIfOne(r: Register, offset: Int) extends Instruction

  case class Program(
      instructions: List[Instruction],
      val register: mutable.Map[String, Int] = mutable.Map(),
      var currentInstruction: Int = 0) {
    def isFinished(): Boolean =
      currentInstruction >= instructions.length

    def execute(): Unit =
      while (!isFinished()) {
        step()
      }

    def get(r: Register): Int =
      register.getOrElseUpdate(r, 0)

    private def nextInstruction(offset: Int = 1): Unit =
      currentInstruction = currentInstruction + offset

    def step(): Unit = {
      if (!isFinished()) {
        instructions(currentInstruction) match {
          case Half(r) =>
            register(r) = get(r) / 2
            nextInstruction()

          case Triple(r) =>
            register(r) = get(r) * 3
            nextInstruction()

          case Increment(r) =>
            register(r) = get(r) + 1
            nextInstruction()

          case Jump(offset) =>
            nextInstruction(offset)

          case JumpIfEven(r, offset) =>
            if (get(r) % 2 == 0) {
              nextInstruction(offset)
            } else {
              nextInstruction()
            }

          case JumpIfOne(r, offset) =>
            if (get(r) == 1) {
              nextInstruction(offset)
            } else {
              nextInstruction()
            }
        }
      }
    }

    override def toString =
      s"Current instruction: $currentInstruction\nRegister: ${register.mkString(", ")}"
  }

  // Input
  def lineToInstruction(line: String): Instruction = {
    val half = """hlf ([A-Za-z]+)""".r
    val triple = """tpl ([A-Za-z]+)""".r
    val inc = """inc ([A-Za-z]+)""".r
    val jump = """jmp ([+-]{0,1}[0-9]+)""".r
    val jie = """jie ([A-Za-z]+), ([+-]{0,1}[0-9]+)""".r
    val jio = """jio ([A-Za-z]+), ([+-]{0,1}[0-9]+)""".r
    line match {
      case half(register)        => Half(register)
      case triple(register)      => Triple(register)
      case inc(register)         => Increment(register)
      case jump(offset)          => Jump(offset.toInt)
      case jie(register, offset) => JumpIfEven(register, offset.toInt)
      case jio(register, offset) => JumpIfOne(register, offset.toInt)
    }
  }
  
  val input = IO.getLines(lineToInstruction)
  val program1 = Program(input)

  program1.execute()
  val b = program1.register("b")
  println(s"b=$b")
}