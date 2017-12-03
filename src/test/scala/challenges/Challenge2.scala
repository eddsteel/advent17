package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge2Test extends FlatSpec {
  "challenge 2" should "follow examples" in {
    assert(Challenge2.run(Star1, Challenge2Test.example1) === "18")
    assert(Challenge2.run(Star2, Challenge2Test.example2) === "9")
  }
}

object Challenge2Test {
  val example1 = """5	1	9	5
                   |7	5	3
                   |2	4	6	8""".stripMargin

  val example2 = """5	9	2	8
                   |9	4	7	3
                   |3	8	6	5""".stripMargin
}
