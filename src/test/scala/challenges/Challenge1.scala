package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge1Test extends FlatSpec {
  "challenge1" should "follow examples" in {
    assert(Challenge1.run(Star1, "1122") === "3")
    assert(Challenge1.run(Star1, "1111") === "4")
    assert(Challenge1.run(Star1, "1234") === "0")
    assert(Challenge1.run(Star1, "91212129") === "9")

    assert(Challenge1.run(Star2, "1212") === "6")
    assert(Challenge1.run(Star2, "1221") === "0")
    assert(Challenge1.run(Star2, "123123") === "12")
    assert(Challenge1.run(Star2, "12131415") === "4")
  }
}
