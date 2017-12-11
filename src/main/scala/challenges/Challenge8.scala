package com.eddsteel.advent17.challenges
import cats.data.NonEmptyList
import cats.implicits._

//
// Each instruction consists of several parts: the
// register to modify, whether to increase or decrease that register's value, the
// amount by which to increase or decrease it, and a condition. If the condition
// fails, skip the instruction without modifying the register. The registers all
// start at 0. The instructions look like this:
//
// b inc 5 if a > 1
// a inc 1 if b < 5
// c dec -10 if a >= 1
// c inc -20 if c == 10
//
object Challenge8 extends Challenge {
  import Parser._
  import Parsers._

  type Registers = Map[Register, Int]
  type Register = String

  def lookup(registers: Registers, register: Register): Int =
    registers.getOrElse(register, 0)

  def update(registers: Registers, register: Register, op: Int => Int): Registers =
    registers.updated(register, op(lookup(registers, register)))

  def test(registers: Registers, register: Register, test: Int => Boolean): Boolean =
    test(lookup(registers, register))

  sealed trait Operation {
    def execute(state: Registers): Registers
  }

  object Operation {
    final case class Inc(register: Register, amount: Int) extends Operation {
      def execute(state: Registers): Registers =
        update(state, register, _ + amount)
    }

    final case class Dec(register: Register, amount: Int) extends Operation {
      def execute(state: Registers): Registers =
        update(state, register, _ - amount)
    }
  }

  sealed trait Condition {
    def eval(state: Registers): Boolean
  }
  object Condition {
    final case class Gt(register: Register, testAmount: Int) extends Condition {
      def eval(state: Registers): Boolean =
        test(state, register, _ > testAmount)
    }
    final case class Lt(register: Register, testAmount: Int) extends Condition {
      def eval(state: Registers): Boolean =
        test(state, register, _ < testAmount)
    }
    final case class Gte(register: Register, testAmount: Int) extends Condition {
      def eval(state: Registers): Boolean =
        test(state, register, _ >= testAmount)
    }
    final case class Lte(register: Register, testAmount: Int) extends Condition {
      def eval(state: Registers): Boolean =
        test(state, register, _ <= testAmount)
    }
    final case class Equ(register: Register, testAmount: Int) extends Condition {
      def eval(state: Registers): Boolean =
        test(state, register, _ === testAmount)
    }
    final case class Neq(register: Register, testAmount: Int) extends Condition {
      def eval(state: Registers): Boolean =
        test(state, register, _ =!= testAmount)
    }
  }

  final case class Instruction(operation: Operation, condition: Condition) {
    def execute(state: Registers): Registers =
      if (condition.eval(state)) operation.execute(state)
      else state
  }

  import Condition._
  import Operation._

  def parseRegister: Parser[Register] = parseAlphas
  def parseSpace: Parser[Unit] = parseString(" ").map(_ => ())

  def parseOperation: Parser[Operation] =
    flattenParser {
      (parseRegister <* parseSpace, parseAlphas, parseSpace *> parseNumber).mapN {
        case (r, "inc", amt) => Some(Inc(r, amt.toInt))
        case (r, "dec", amt) => Some(Dec(r, amt.toInt))
        case _               => None
      }
    }

  def parseCondition: Parser[Condition] =
    flattenParser {
      (parseString("if ") *> parseRegister, parseRegex(" ..? ".r), parseNumber).mapN {
        case (r, " > ", l)  => Some(Gt(r, l.toInt))
        case (r, " < ", l)  => Some(Lt(r, l.toInt))
        case (r, " >= ", l) => Some(Gte(r, l.toInt))
        case (r, " <= ", l) => Some(Lte(r, l.toInt))
        case (r, " == ", l) => Some(Equ(r, l.toInt))
        case (r, " != ", l) => Some(Neq(r, l.toInt))
        case _              => None
      }
    }

  def parseLine: Parser[Instruction] =
    (parseOperation <* parseSpace, parseCondition).mapN {
      case (o, c) => Instruction(o, c)
    }

  def largestValue(state: Registers): Int = {
    val vals = state.values
    if (vals.isEmpty) 0
    else vals.max
  }

  def parseInstructions(input: String): ErrorOr[NonEmptyList[Instruction]] =
    input
      .split("\n")
      .map(s => parseLine(s).map(_._1))
      .toList
      .sequence
      .flatMap(NonEmptyList.fromList)
      .toRight("Bad Input!")

  def runInstructionsWithOp[A](
    in: NonEmptyList[Instruction],
    initial: (Registers, A),
    op: (Registers, A) => A): (Registers, A) =
    in.foldLeft(initial) {
      case ((registers, a), next) =>
        val nextA = op(registers, a)
        next.execute(registers) -> nextA
    }

  def runInstructions(in: NonEmptyList[Instruction], initial: Registers): Registers =
    runInstructionsWithOp[Unit](in, (initial, ()), { case _ => () })._1

  def runFromEmptyLastMax(in: NonEmptyList[Instruction]): Int =
    largestValue(runInstructions(in, Map.empty[Register, Int]))

  def runFromEmptyMax(in: NonEmptyList[Instruction]): Int =
    runInstructionsWithOp[Int](in, (Map.empty[Register, Int], 0), {
      case (r: Registers, i: Int) => i.max(largestValue(r))
    })._2

  def run(star: Star, input: String): String = {
    val op = star.switch(runFromEmptyLastMax _, runFromEmptyMax _)
    parseInstructions(input).map(op).show
  }
}
