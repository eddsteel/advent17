package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge16Test extends FlatSpec with Matchers with OptionValues {
  import Challenge16.{run => _, _}
  val s1: State = "abcde".toVector

  // For example, with only five programs standing in a line (abcde),
  "Challenge 16" should "follow examples" in {
    // s1, a spin of size 1: eabcd.
    val s2 = spin(s1, 1).toOption.value
    s2.shouldEqual("eabcd".toVector)
    // x3/4, swapping the last two programs: eabdc
    val s3 = exchange(s2, 3, 4).toOption.value
    s3.shouldEqual("eabdc".toVector)
    // pe/b, swapping programs e and b: baedc.
    partner(s3, 'e', 'b').shouldEqual(Right("baedc".toVector))

    runInstructions(s1, List(Spin(1), Exchange(3, 4), Partner('e', 'b')))
      .shouldEqual(Right("baedc".toVector))

    val exampleInput = "s1,x3/4,pe/b"

    readInstructions(exampleInput)
      .flatMap(runInstructions(s1, _))
      .shouldEqual(Right("baedc".toVector))

    readInstructions(exampleInput)
      .flatMap(runInstructions("baedc".toVector, _))
      .shouldEqual(Right("ceadb".toVector))

  }
}
