package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

object Challenge15 extends Challenge {
  import Parser._, Parsers._

  def binarize(value: Long): String =
    Util.padStringL(32, '0')(value.toBinaryString)

  class Generator(factor: Int) {
    def generateNext(last: Long): Long =
      (last * factor).toLong % 2147483647L

    def generate(initial: Long): Stream[Long] =
      Stream.iterate(initial)(generateNext).drop(1)

    def generateBinary(initial: Long): Stream[String] =
      generate(initial).map(binarize)
  }

  val GeneratorA: Generator = new Generator(16807)
  val GeneratorB: Generator = new Generator(48271)

  def countSignificance(pairs: Stream[(String, String)], length: Int): Int =
    pairs.take(length).count {
      case (stringA, stringB) => stringA.takeRight(16) === stringB.takeRight(16)
    }

  def generatePairs(aStart: Long, bStart: Long): Stream[(String, String)] =
    GeneratorA.generateBinary(aStart).zip(GeneratorB.generateBinary(bStart))

  def generateFilteredPairs(aStart: Long, bStart: Long): Stream[(String, String)] = {
    def filterA(in: Long): Boolean = in % 4 === 0
    def filterB(in: Long): Boolean = in % 8 === 0

    GeneratorA
      .generate(aStart)
      .filter(filterA)
      .map(binarize)
      .zip(GeneratorB.generate(bStart).filter(filterB).map(binarize))
  }

  def parseInput: Parser[(Long, Long)] =
    (
      parseString("Generator A starts with ") *> parseNumber <* parseString("\n"),
      parseString("Generator B starts with ") *> parseNumber).mapN {
      case (a, b) => (a, b)
    }

  def run(star: Star, input: String): String = {
    val parsedInput = parseInput(input)
    Either
      .fromOption(parsedInput, "Bad input!")
      .map {
        case ((a, b), _) =>
          star.switch({
            val pairs = generatePairs(a, b)
            countSignificance(pairs, 40000000)
          }, {
            val pairs = generateFilteredPairs(a, b)
            countSignificance(pairs, 5000000)
          })
      }
      .fold(identity, _.toString)

  }

}
