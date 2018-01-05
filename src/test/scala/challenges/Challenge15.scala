package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge15Test extends FlatSpec with Matchers with OptionValues {
  import Challenge15.{run => _, _}

  "Examples" should "be right." in {
    GeneratorA
      .generate(65L)
      .take(5)
      .shouldEqual(Stream(1092455L, 1181022009L, 245556042L, 1744312007L, 1352636452))

    GeneratorB
      .generate(8921L)
      .take(5)
      .shouldEqual(Stream(430625591L, 1233683848L, 1431495498L, 137874439L, 285222916))

    GeneratorA
      .generateBinary(65L)
      .take(5)
      .shouldEqual(Stream(
        "00000000000100001010101101100111",
        "01000110011001001111011100111001",
        "00001110101000101110001101001010",
        "01100111111110000001011011000111",
        "01010000100111111001100000100100"
      ))

    GeneratorB
      .generateBinary(8921L)
      .take(5)
      .shouldEqual(Stream(
        "00011001101010101101001100110111",
        "01001001100010001000010110001000",
        "01010101010100101110001101001010",
        "00001000001101111100110000000111",
        "00010001000000000010100000000100"
      ))

    countSignificance(generatePairs(65L, 8921L), 5).shouldEqual(1)
  }

  "input parser" should "work" in {
    val input = """Generator A starts with 1
                  |Generator B starts with 2""".stripMargin
    parseInput(input).shouldEqual(Some(((1L, 2L), "")))
  }

  /*
--Gen. A--  --Gen. B--
1092455       430625591
1181022009   1233683848
245556042    1431495498
1744312007    137874439
1352636452    285222916
   */

  /*

 */
}
