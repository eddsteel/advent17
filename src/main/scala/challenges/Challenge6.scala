package com.eddsteel.advent17.challenges
import _root_.cats.data.NonEmptyVector
import _root_.cats.implicits._

object Challenge6 extends Challenge {

  type Bank = Int
  type Banks = NonEmptyVector[Bank]

  def zeroes(length: Int): Banks =
    NonEmptyVector(0, Vector.fill(length - 1)(0))

  def ones(length: Int): Banks =
    NonEmptyVector(1, Vector.fill(length - 1)(1))

  def biggest(banks: Banks): (Bank, Int) =
    banks.toVector.zipWithIndex.maxBy(_._1)

  def unsafeOp[A](v: NonEmptyVector[A])(op: Vector[A] => Vector[A]): NonEmptyVector[A] =
    NonEmptyVector.fromVectorUnsafe(op(v.toVector))

  def padToMultiple[A](v: NonEmptyVector[A], factor: Int, value: A): NonEmptyVector[A] =
    unsafeOp(v) { v =>
      val ln = v.length
      val padToLn = ln + factor - ((ln + factor) % factor)
      v.padTo(padToLn, value)
    }

  def cycle(banks: Banks): Banks = {
    val (mem, idx) = biggest(banks)
    val initial = banks.updatedUnsafe(idx, 0)

    unsafeOp(padToMultiple(initial |+| zeroes(idx + 1) |+| ones(mem), banks.length, 0)) {
      _.grouped(banks.length).toVector.transpose.map(_.combineAll)
    }
  }

  def streamFrom(banks: Banks): Stream[Banks] =
    Stream.iterate(banks)(cycle)

  def streamToDupe(banks: Banks): Option[(Int, Banks)] =
    streamFrom(banks)
      .scanLeft(Set.empty[Banks])(_ + _)
      .zip(streamFrom(banks))
      .zipWithIndex
      .collectFirst {
        case ((seen, banks), i) if seen.contains(banks) => (i, banks)
      }

  def streamToNext(banks: Banks): Option[Int] =
    streamFrom(banks).drop(1).zipWithIndex.collectFirst {
      case (`banks`, i) => i
    }

  def solve(banks: Banks): Option[Int] =
    streamToDupe(banks).map(_._1)

  def solve2(banks: Banks): Option[Int] =
    for {
      (_, bs) <- streamToDupe(banks)
      i <- streamToNext(bs)
    } yield i + 1

  def run(star: Star, input: String): String =
    (for {
      vec <- NonEmptyVector.fromVector(input.split("\t").toVector).toRight("Bad input!")
      solver = star.switch(solve _, solve2 _)
      banks <- vec
        .map(s => Either.catchNonFatal(s.toInt).leftMap(_ => "Bad number!"))
        .sequence[Either[String, ?], Int]
      soln <- solver(banks).toRight("Couldn't solve!'")
    } yield soln).fold(identity, _.show)
}
