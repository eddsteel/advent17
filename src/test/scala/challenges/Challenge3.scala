package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge3Test extends FlatSpec {
  "challenge 3" should "follow examples" in {
    assert(Challenge3.blockDistance(1) === Right(0))
    assert(Challenge3.blockDistance(12) === Right(3))
    assert(Challenge3.blockDistance(23) === Right(2))
    assert(Challenge3.blockDistance(1024) === Right(31))

    val grid = Challenge3.Grid.from((0, 0), (1, 0), (1, 1), (0, 1), (-1, 1))
    println(grid)
    assert(grid.valueAt((0, 0)) === 1)
    assert(grid.valueAt((1, 0)) === 1)
    assert(grid.valueAt((1, 1)) === 2)
    assert(grid.valueAt((0, 1)) === 4)
    assert(grid.valueAt((-1, 1)) === 5)
  }
}
