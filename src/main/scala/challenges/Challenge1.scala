package com.eddsteel.advent17.challenges
import _root_.cats.data.NonEmptyList
import _root_.cats.implicits._

object Challenge1 extends Challenge {
  def run(star: Star, input: String): String = {

    def otherNumbers[A](numbers: NonEmptyList[A]): Option[NonEmptyList[A]] =
      star.switch(
        Some(NonEmptyList.ofInitLast(numbers.tail, numbers.head)),
        NonEmptyList.fromList(
          numbers.toList.drop(numbers.length / 2) |+| numbers.toList.take(numbers.length / 2))
      )

    val resultOrError = for {
      nums <- Util.parseInts(input.map(_.toString).toList)
      numNel <- NonEmptyList.fromList(nums).toRight("Empty input!")
      others <- otherNumbers(numNel).toRight("Wrong size of list!")
      pairs = nums.zip(others.toList)
    } yield
      pairs.foldMap {
        case (i: Int, j: Int) if i === j => i
        case _                           => 0
      }

    resultOrError.fold(identity, _.show)
  }
}
