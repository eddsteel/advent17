package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

//
object Challenge13 extends Challenge {
  import Parser._, Parsers._

  final case class Layer(depth: Int, range: Int) {
    def scannerAtTopWhenVisited: Boolean =
      scannerAtTopWithDelay(0)

    def scannerAtTopWithDelay(delay: Int): Boolean =
      (delay + depth) % (range + range - 2) === 0

  }

  // Option because getting caught at layer 0 is severity 0
  def severity(layer: Layer, delay: Int): Option[Int] =
    Option(layer).collect {
      case l @ Layer(d, r) if l.scannerAtTopWithDelay(delay) =>
        d * r
    }

  def tripSeverity(layers: List[Layer]): Int =
    tripSeverityWithDelay(0, layers).getOrElse(0)

  def tripSeverityWithDelay(delay: Int, layers: List[Layer]): Option[Int] =
    layers.foldMap(severity(_, delay))

  def findDelay(layers: List[Layer]): Int =
    (1 to Int.MaxValue)
//      .drop(2613824) // this is the 2nd number I got... 3rd was correct >:(
    .find { i =>
      tripSeverityWithDelay(i, layers).isEmpty
    }.getOrElse(0)

  def parseLayer: Parser[Layer] =
    (parseNumber, (parseString(": ") *> parseNumber)).mapN {
      case (d, r) => Layer(d.toInt, r.toInt)
    }

  def parseLayers: Parser[List[Layer]] =
    parseMany(parseLayer <* parseString("\n"))

  def readInput(input: String): ErrorOr[List[Layer]] =
    Either.fromOption(
      parseLayers(input).map(_._1),
      "Bad input!"
    )

  def run(star: Star, input: String): String =
    readInput(input).map { i =>
      star.switch(tripSeverity _, findDelay _)(i)
    }.fold(identity, _.show)
}
