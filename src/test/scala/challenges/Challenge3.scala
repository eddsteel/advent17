package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge3Test extends FlatSpec {
  "challenge 3" should "follow examples" in {
    assert(Challenge3.blockDistance(1) === Some(0))
    assert(Challenge3.blockDistance(12) === Some(3))
    assert(Challenge3.blockDistance(23) === Some(2))
    assert(Challenge3.blockDistance(1024) === Some(31))

    val grid = Challenge3.Grid.from((0, 0), (1, 0), (1, 1), (0, 1), (-1, 1))
    assert(grid.valueAt((0, 0)) === 1)
    assert(grid.valueAt((1, 0)) === 1)
    assert(grid.valueAt((1, 1)) === 2)
    assert(grid.valueAt((0, 1)) === 4)
    assert(grid.valueAt((-1, 1)) === 5)
  }
}
