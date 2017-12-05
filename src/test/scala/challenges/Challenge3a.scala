package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge3aTest extends FlatSpec {
  import Challenge3a.{run => _, _}
  "challenge 3" should "follow examples" in {
    assert(blockDistance(1) === 0)
    assert(blockDistance(12) === 3)
    assert(blockDistance(23) === 2)
    assert(blockDistance(1024) === 31)
  }

  "distance along ordinals" should "be correct" in {
    assert(distanceOrdinalD(28, East) === 3.0) // E
    assert(distanceOrdinalD(34, North) === 3.0) // N
    assert(distanceOrdinalD(19, West) === 2.0) // W
    assert(distanceOrdinalD(8, South) === 1.0) // S
  }

}
