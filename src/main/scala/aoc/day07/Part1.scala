package aoc.day07

import io.IO

object Part1 extends App {
  /*
--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise 
logic gates! Unfortunately, little Bobby is a little under the recommended
age range, and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit
signal (a number from 0 to 65535). A signal is provided to each wire by a 
gate, another wire, or some specific value. Each wire can only get a signal
from one source, but can provide its signal to multiple destinations. A gate
provides no signal until all of its inputs have a signal.

The included instructions booklet describe how to connect the parts together:
x AND y -> z means to connect wires x and y to an AND gate, and then connect 
its output to wire z.

For example:

    123 -> x means that the signal 123 is provided to wire x.
    x AND y -> z means that the bitwise AND of wire x and wire y is provided
    to wire z.
    p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and
    then provided to wire q.
    NOT e -> f means that the bitwise complement of the value from wire e is
    provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for
some reason, you'd like to emulate the circuit instead, almost all programming
languages (for example, C, JavaScript, or Python) provide operators for these
gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456

In little Bobby's kit's instructions booklet (provided as your puzzle input), 
what signal is ultimately provided to wire a?
 */
  object WireBox {
    private var registerDependencies = Map[String, Expr]() // Register dependencies between variables
    private var registerValue = Map[String, Int]() // Register computation of values of the variables
    private var needRecomputation = true // Flag to note when a recomputation is needed (i.e., a variable is added or modified)

    // Computation stack
    def addToRegister(vName: String, e: Expr): Unit = {
      registerDependencies += (vName -> e)
      needRecomputation = true
    }

    def variables: Set[String] =
      registerDependencies.keys.toSet

    def apply(varName: String): Int =
      eval(registerDependencies(varName))

    def assignement(expr: Expr, variable: Variable): Unit =
      addToRegister(variable.name, expr)

    private var evaluationStack = List[String]()
    private def stack(name: String): Unit =
      evaluationStack = name :: evaluationStack
    private def pop(): Unit =
      evaluationStack = evaluationStack.tail
    private def reset(): Unit =
      evaluationStack = List[String]()

    def eval(e: Expr): Int = {
      def evalHelper(e: Expr): Int =
        e match {
          case Value(v) => v
          case AND(x, y) =>
            evalHelper(x) & evalHelper(y)
          case OR(x, y) =>
            evalHelper(x) | evalHelper(y)
          case LSHIFT(x: Expr, shift: Int) =>
            evalHelper(x) << shift
          case RSHIFT(x, shift) =>
            evalHelper(x) >> shift
          case NOT(x: Expr) =>
            ~evalHelper(x)
          case Variable(name) =>
            if (registerValue.contains(name)) {
              // Already evaluated
              registerValue(name)
            } else {
              // Evaluate
              if (evaluationStack.contains(name)) {
                // Cyclic dependency detected
                throw new Exception(s"Cyclic dependency detected with $name")
              } else {
                val ref = registerDependencies(name) // Return NaN in case of bad reference
                // Stacking of the ref name that is going to be computed
                stack(name)
                // Evaluation of its value
                val value = evalHelper(ref)
                registerValue += (name -> value)
                // Remove the ref name from the stack (it is now evaluated)
                pop()
                value
              }
            }
        }

      // Clean the computation
      if (needRecomputation) {
        reset() // Reset stack
        registerValue = Map[String, Int]()
        needRecomputation = false
      }

      // Eval
      evalHelper(e)
    }

    sealed abstract class Expr
    case class Value(v: Int) extends Expr
    case class Variable(name: String) extends Expr
    case class AND(a: Expr, b: Expr) extends Expr
    case class OR(a: Expr, b: Expr) extends Expr
    case class LSHIFT(a: Expr, shift: Int) extends Expr
    case class RSHIFT(a: Expr, shift: Int) extends Expr
    case class NOT(x: Expr) extends Expr

    override def toString =
      s"WireBox(size=${registerDependencies.size})"
  }

  def stringToAssignementExecution(line: String): Unit = {
    import WireBox._
    val assignementR = """([a-zA-Z]+|[0-9]+) -> ([a-zA-Z]+)""".r
    val binaryOpR = """([a-zA-Z]+|[0-9]+) (AND|OR) ([a-zA-Z]+|[0-9]+) -> ([a-zA-Z]+)""".r
    val shiftR = """([a-zA-Z]+) (LSHIFT|RSHIFT) ([0-9]+) -> ([a-zA-Z]+)""".r
    val notR = """NOT ([a-zA-Z]+) -> ([a-zA-Z]+)""".r

    def stringToExpr(item: String): Expr =
      if (item forall Character.isDigit) {
        Value(item.toInt)
      } else {
        Variable(item)
      }

    line match {
      case assignementR(expr, variable) =>
        assignement(stringToExpr(expr), Variable(variable))

      case binaryOpR(expr1, op, expr2, vAssign) =>
        val operation = op match {
          case "AND" => AND(stringToExpr(expr1), stringToExpr(expr2))
          case "OR" => OR(stringToExpr(expr1), stringToExpr(expr2))
        }
        assignement(operation, Variable(vAssign))

      case shiftR(v1, op, value, vAssign) =>
        val operation = op match {
          case "RSHIFT" => RSHIFT(Variable(v1), value.toInt)
          case "LSHIFT" => LSHIFT(Variable(v1), value.toInt)
        }
        assignement(operation, Variable(vAssign))

      case notR(v1, v2) =>
        assignement(NOT(Variable(v1)), Variable(v2))

    }
  }
  
  val input = IO.getLines()
  for (op <- input) {
    stringToAssignementExecution(op)
  }
  println(s"Signal provided to a: ${WireBox("a")}")  
}