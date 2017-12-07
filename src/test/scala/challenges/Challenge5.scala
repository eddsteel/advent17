package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge5Test extends FlatSpec {
  import Challenge5.{run => _, _}
  "challenge 5" should "follow examples" in {
    val i = Vector(0, 3, 0, 1, -3)
    val p = 0

    val (p2, i2) = jump(p, i, _ => 1)
    val (p3, i3) = jump(p2, i2, _ => 1)
    val (p4, i4) = jump(p3, i3, _ => 1)
    val (p5, i5) = jump(p4, i4, _ => 1)
    val (p6, i6) = jump(p5, i5, _ => 1)

    assert((p2, i2).===((0, Vector(1, 3, 0, 1, -3))))
    assert((p3, i3).===((1, Vector(2, 3, 0, 1, -3))))
    assert((p4, i4).===((4, Vector(2, 4, 0, 1, -3))))
    assert((p5, i5).===((1, Vector(2, 4, 0, 1, -2))))
    assert((p6, i6).===((5, Vector(2, 5, 0, 1, -2))))

    assert(runToCompletion(_ => 1)(i).===(Some((5, (5, Vector(2, 5, 0, 1, -2)), true))))
    assert(
      runToCompletion(i => if (i >= 3) -1 else 1)(i)
        .===(Some((10, (5, Vector(2, 3, 2, 3, -1)), true))))
  }
}
