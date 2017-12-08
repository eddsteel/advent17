package com.eddsteel.advent17.challenges
import _root_.cats.Eq
import _root_.cats.data.NonEmptyVector
import _root_.cats.implicits._
import _root_.org.scalatest._

class Challenge6Test extends FlatSpec with Matchers {
  import Challenge6.{run => _, _}

  implicit val eqBank: Eq[Bank] = implicitly[Eq[Int]]

  "challenge 6" should "follow examples" in {

    assert(cycle(NonEmptyVector.of(0, 2, 7, 0)) === NonEmptyVector.of(2, 4, 1, 2))
    assert(cycle(NonEmptyVector.of(2, 4, 1, 2)) === NonEmptyVector.of(3, 1, 2, 3))
    assert(cycle(NonEmptyVector.of(3, 1, 2, 3)) === NonEmptyVector.of(0, 2, 3, 4))
    assert(cycle(NonEmptyVector.of(0, 2, 3, 4)) === NonEmptyVector.of(1, 3, 4, 1))
    assert(cycle(NonEmptyVector.of(1, 3, 4, 1)) === NonEmptyVector.of(2, 4, 1, 2))

    solve(NonEmptyVector.of(0, 2, 7, 0)).shouldEqual(Some(5))

    solve2(NonEmptyVector.of(0, 2, 7, 0)).shouldEqual(Some(4))
  }
}
