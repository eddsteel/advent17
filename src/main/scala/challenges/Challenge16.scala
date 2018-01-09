package com.eddsteel.advent17.challenges
import _root_.cats.implicits._
import _root_.cats.data.NonEmptyList

object Challenge16 extends Challenge {
  import Parser._, Parsers._

  type Name = Char
  type State = Vector[Name]

  sealed trait Instruction
  final case class Spin(amt: Int) extends Instruction
  final case class Exchange(pos1: Int, pos2: Int) extends Instruction
  final case class Partner(prog1: Name, prog2: Name) extends Instruction

  def spin(state: State, amt: Int): ErrorOr[State] = {
    def recombine(tuple: (List[Name], List[Name])): State =
      (tuple match {
        case (a, b) => b ::: a
      }).toVector

    Either.catchNonFatal(recombine(state.toList.splitAt(state.length - amt))).leftMap(_.getMessage)
  }

  def fetch(state: State, pos: Int): ErrorOr[Name] =
    Either.catchNonFatal(state(pos)).leftMap(_.getMessage)

  def find(state: State, name: Name): ErrorOr[Int] =
    Either.catchNonFatal(state.indexOf(name)).leftMap(_.getMessage)

  def exchange(state: State, pos1: Int, pos2: Int): ErrorOr[State] =
    (fetch(state, pos1), fetch(state, pos2)).mapN {
      case (prog1, prog2) => state.updated(pos1, prog2).updated(pos2, prog1)
    }

  def partner(state: State, prog1: Name, prog2: Name): ErrorOr[State] =
    (find(state, prog1), find(state, prog2)).mapN {
      case (pos1, pos2) => state.updated(pos1, prog2).updated(pos2, prog1)
    }

  def execute(state: State, instruction: Instruction): ErrorOr[State] = instruction match {
    case Spin(amt)             => spin(state, amt)
    case Exchange(pos1, pos2)  => exchange(state, pos1, pos2)
    case Partner(prog1, prog2) => partner(state, prog1, prog2)
  }

  val parseSpin: Parser[Instruction] =
    (parseChar('s') *> parseNumber).map(l => Spin(l.toInt))

  val parseExchange: Parser[Instruction] =
    (parseChar('x') *> parseNumber, parseChar('/') *> parseNumber).mapN {
      case (a, b) => Exchange(a.toInt, b.toInt)
    }

  val parsePartner: Parser[Instruction] =
    (parseChar('p') *> parseAnyChar, parseChar('/') *> parseAnyChar).mapN {
      case (a, b) => Partner(a, b)
    }

  val parseInstruction: Parser[Instruction] =
    parseSpin.or(parseExchange).or(parsePartner)

  val parseInstructions: Parser[NonEmptyList[Instruction]] =
    s => {
      val instructions = s.split(",").toList.flatMap(i => parseInstruction(i).map(_._1).toList)
      NonEmptyList.fromList(instructions).map((_, ""))
    }

  def readInstructions(input: String): ErrorOr[List[Instruction]] =
    Either.fromOption(parseInstructions(input).map(_._1.toList), "Bad input!")

  def runInstructions(state: State, instructions: List[Instruction]): ErrorOr[State] =
    instructions.foldLeftM(state)(execute)

  val initialState: State = ('a' to 'p').toVector

  @annotation.tailrec
  def findLoop(
    startState: State,
    visited: Set[State],
    counter: Int,
    instructions: List[Instruction]): ErrorOr[(Int, State)] = {
    val nextState = runInstructions(startState, instructions)
    val nextCounter = counter + 1

    nextState match {
      case Right(s) if (visited.contains(s)) => (nextCounter -> s).asRight[String]
      case Right(s)                          => findLoop(s, visited + s, nextCounter, instructions)
      case Left(s)                           => (s).asLeft[(Int, State)]
    }
  }

  @annotation.tailrec
  def runRepeatedlyM[A, M[_]: cats.Monad](repeat: Long, a: M[A], f: A => M[A]): M[A] =
    if (repeat === 0) a
    else {
      if (repeat % 10000 === 0) println(repeat)
      runRepeatedlyM(repeat - 1, a.flatMap(f), f)
    }

  // fake a billion by finding the loop, getting the last time this loop would occur (1b mod loop length)
  // and just iterating the last few times.
  //
  def billionIterations(state: State, instructions: List[Instruction]): ErrorOr[State] = {
    val resultOrError = findLoop(state, Set.empty, 0, instructions)
    resultOrError.flatMap {
      case (counter, state) =>
        val requiredIterations = 1000000000L % counter
        runRepeatedlyM(
          requiredIterations + 1,
          state.pure[ErrorOr],
          (s: State) => runInstructions(s, instructions))
    }
  }

  def run(star: Star, input: String): String =
    readInstructions(input).flatMap { i =>
      star.switch(runInstructions(initialState, i), billionIterations(initialState, i))
    }.fold(identity, _.mkString)
}
