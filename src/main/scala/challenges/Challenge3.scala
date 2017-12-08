package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

/* Second go at challenge3, actually walking the spiral to accumulate state.
 */
object Challenge3 extends Challenge {
  type Coords = (Int, Int) // TODO refine
  type Direction = Char // TODO refine
  // TODO state based  soln

  implicit class CoordsSyntax(coords: Coords) {
    val x: Int = coords._1
    val y: Int = coords._2

    def neighbours: Seq[Coords] =
      for {
        a <- Range(x - 1, x + 2)
        b <- Range(y - 1, y + 2)
        if a =!= x || b =!= y
      } yield (a, b)

    def bd: Int =
      Math.abs(x) + Math.abs(y)

    def move(d: Direction): Coords =
      d match {
        case 'E' => (x + 1, y)
        case 'N' => (x, y + 1)
        case 'W' => (x - 1, y)
        case 'S' => (x, y - 1)
        case _   => (x, y)
      }
  }

  final case class Grid(cells: Map[Coords, Int], last: Int) {
    def valueAt(c: Coords): Int =
      cells.getOrElse(c, 0)

    def valueOfNeighbours(c: Coords): Int =
      c.neighbours.toList.foldMap(valueAt)

    def add(c: Coords): Grid = {
      val value = valueOfNeighbours(c).max(1)
      Grid(cells.updated(c, value), value)
    }
  }

  object Grid {
    def empty: Grid = new Grid(Map.empty[Coords, Int], 0)
    def from(vals: Coords*): Grid =
      vals.foldLeft(empty)(_.add(_))
  }

  /** Path expressed as endless stream of instructions. */
  val spiralPath: Stream[Direction] = {
    val counts: Stream[Int] = Stream.from(1).flatMap(x => Stream(x, x))
    val ordinals: Stream[Direction] = Stream.continually("ENWS".toStream).flatten

    counts.zip(ordinals).flatMap {
      case (c, o) => Stream.fill(c)(o)
    }
  }

  // builds a spiral: stream of coordinates where (0,0) is the centre
  def spiralCoords: Stream[Coords] =
    spiralPath.scanLeft((0, 0))(_.move(_))

  // Block distance alg.
  // Walk spiral up to number indicated, maintaining current coord state.
  // Block distance is abs(x) + abs(y)
  //
  def blockDistance(number: Int): Option[Int] =
    spiralCoords.drop(number - 1).headOption.map(_.bd)

  // First larger value
  //
  def firstLarger(number: Int): Option[Int] =
    spiralCoords.scanLeft(Grid.empty)(_.add(_)).collectFirst {
      case grid if grid.last > number => grid.last
    }

  def run(star: Star, input: String): String = {
    val solver = star.switch(blockDistance _, firstLarger _)

    Either
      .catchNonFatal(input.toInt)
      .leftMap(_ => "invalid input!")
      .flatMap(in => solver(in).toRight("Didn't find solution"))
      .fold(identity, _.show)
  }
}

/*
36  35  34  33  32  31
17  16  15  14  13  30
18   5   4   3  12  29
19   6   1   2  11  28
20   7   8   9  10  27
21  22  23  24  25  26

 progression of path from [1 (0, 0)]:

1 2 3 4 5 6 7 8 9 ...
  E N W W S S E E E N N N W W W W S S S S E E E E E N N N N N W W W W W W
  1 1 2   2   3     3     4       4       5         5         6
| | | | | (-1, 0) - distance 1
| | | | (-1, 1)   - distance 2
| | | (0, 1)      - distance 1
| | (1, 1)        - distance 2
| (1, 0)          - distance 1
(0,0)             - distance 0

progress along this with an intermediate grid as state.


state monad.
Star 1 is current coords (final state x + y is the block distance after as many steps as input)
Star 2 is current grid and current coords (final state is the sum of surrounding squares that adds up to more than input)
 */
