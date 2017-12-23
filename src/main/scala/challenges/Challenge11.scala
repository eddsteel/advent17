package com.eddsteel.advent17.challenges
import _root_.cats.{Eq, Order}
import _root_.cats.data.NonEmptyList
import _root_.cats.derive
import _root_.cats.implicits._

//
object Challenge11 extends Challenge {
  import Parser._, Parsers._

  /*
  _/N\_
 NW\_/NE
 \_/N\_/
 / \M/ \
 SW/ \ SE
    S
   */
  sealed trait Move

  case object N extends Move
  case object NE extends Move
  case object SE extends Move
  case object S extends Move
  case object SW extends Move
  case object NW extends Move
  case object NoMove extends Move

  object Move {
    @SuppressWarnings(Array("org.wartremover.warts.Equals"))
    implicit val eqMove: Eq[Move] = derive.eq[Move]
    implicit val ordMove: Order[Move] = Order.by[Move, Int] {
      case NoMove => 0
      case N      => 1
      case NE     => 2
      case SE     => 3
      case S      => 4
      case SW     => 5
      case NW     => 6
    }
  }

  def maybeCombine(a: Move, b: Move): Option[Move] = (a.min(b), a.max(b)) match {
    case (N, SE)     => Some(NE)
    case (N, S)      => Some(NoMove)
    case (N, SW)     => Some(NW)
    case (NE, S)     => Some(SE)
    case (NE, SW)    => Some(NoMove)
    case (NE, NW)    => Some(N)
    case (SE, SW)    => Some(S)
    case (SE, NW)    => Some(NoMove)
    case (S, NW)     => Some(SW)
    case (NoMove, m) => Some(m)
    case _           => None
  }

  @annotation.tailrec
  def combineFirst(a: Move, done: List[Move], candidates: List[Move]): NonEmptyList[Move] =
    candidates match {
      case Nil => NonEmptyList.ofInitLast(done, a)
      case c :: cs =>
        maybeCombine(a, c) match {
          case Some(reduced) =>
            NonEmptyList(reduced, cs ::: done.reverse)
          case None =>
            combineFirst(a, c :: done, cs)
        }
    }

  @annotation.tailrec
  def combineAll(moves: NonEmptyList[Move]): NonEmptyList[Move] = moves match {
    case NonEmptyList(_, Nil) => moves
    case NonEmptyList(a, bs) =>
      val reduced = combineFirst(a, Nil, bs)
      if (reduced.length >= moves.length) moves
      else combineAll(reduced)
  }

  def moveDistance(moves: NonEmptyList[Move]): Int =
    moves.filterNot(_ === NoMove).length

  def parseMove: Parser[Move] =
    parseRegex("[ns][ew]?".r).map {
      case "n"  => N
      case "ne" => NE
      case "se" => SE
      case "s"  => S
      case "sw" => SW
      case "nw" => NW
    }

  def relativeMoveDistances(moves: NonEmptyList[Move]): List[Int] = {
    val (allLengths, _) = moves.toList.foldLeft((List.empty[Int], List.empty[Move])) {
      case ((lengths, state), next) =>
        val nextState = combineAll(NonEmptyList(next, state)).toList
        (nextState.filterNot(_ === NoMove).length :: lengths, nextState)
    }

    allLengths
  }

  def run(star: Star, input: String): String = {
    val moves =
      NonEmptyList.fromList(input.split(",").map(parseMove).map(_.toList).flatten.map(_._1).toList)

    val results = for {
      ms <- Either.fromOption(moves, "Bad input")
      distances = relativeMoveDistances(ms)
      head = distances.headOption.getOrElse(0)
      max = distances.max
    } yield (head, max)

    val result: Either[String, Int] = results.map {
      case (star1, star2) =>
        star.switch(star1, star2)
    }

    result.fold(identity, _.show)
  }
}
