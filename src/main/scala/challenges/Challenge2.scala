package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

object Challenge2 extends Challenge {
  def run(star: Star, input: String): String = {
    type Row = List[Int]

    /** Determines behaviour based on star.
      * star 1: find biggest diff between numbers
      * star 2: find two values that divide. We'll assume it's quicker to stop at the first than dedupe
      */
    val rowAggregate: Row => Option[Int] = star.switch(
      row => (row.maximumOption, row.minimumOption).mapN(_ - _),
      row =>
        (row, row).mapN(List(_, _).sorted).collectFirst {
          case List(j, i) if i =!= j && i % j === 0 => i / j
      }
    )

    val parsedNumbers: ErrorOr[List[Row]] =
      input
        .split("\n")
        .toList
        .map(row => Util.parseInts(row.split("\t").toList))
        .sequence[ErrorOr, List[Int]]

    val resultOrError: ErrorOr[Int] = parsedNumbers.flatMap(_.foldMap { row =>
      rowAggregate(row).toRight("invalid row!")
    })

    resultOrError.fold(identity, _.show)
  }
}
