package com.eddsteel.advent17.challenges
import _root_.org.scalatest._
import _root_.cats.data.NonEmptyList

class Challenge11Test extends FlatSpec with Matchers with OptionValues {
  import Challenge11.{run => _, _}

  /*
   ne,ne,ne is 3 steps away.
   ne,ne,sw,sw is 0 steps away (back where you started).
   ne,ne,s,s is 2 steps away (se,se).
   se,sw,se,sw,sw is 3 steps away (s,s,sw).
   */
  "Challenge 11" should "work like examples" in {
    relativeMoveDistances(NonEmptyList.of(NE, NE, NE)).headOption.shouldEqual(Some(3))
    relativeMoveDistances(NonEmptyList.of(NE, NE, SW, SW)).headOption.shouldEqual(Some(0))
    relativeMoveDistances(NonEmptyList.of(SE, SW, SE, SW, SW)).headOption.shouldEqual(Some(3))
    relativeMoveDistances(NonEmptyList.of(NE, NE, S, S)).headOption.shouldEqual(Some(2))
  }
}
