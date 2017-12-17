package com.eddsteel.advent17.challenges
import _root_.cats.implicits._
import _root_.org.scalatest._

class Challenge8Test extends FlatSpec with Matchers {
  import Challenge8.{run => _, _}
  import Challenge8.Operation._
  import Challenge8.Condition._

  val input = """b inc 5 if a > 1
                |a inc 1 if b < 5
                |c dec -10 if a >= 1
                |c inc -20 if c == 10""".stripMargin

  "challenge 8" should "follow examples" in {
    Instruction(Inc("b", 5), Gt("a", 1))
      .execute(Map.empty[Register, Int])
      .shouldEqual(Map.empty[Register, Int])

    Instruction(Inc("a", 1), Lt("b", 5))
      .execute(Map.empty[Register, Int])
      .shouldEqual(Map("a" -> 1))

    Instruction(Dec("c", -10), Gte("a", 1))
      .execute(Map("a" -> 1))
      .shouldEqual(Map("a" -> 1, "c" -> 10))

    Instruction(Inc("c", -20), Equ("c", 10))
      .execute(Map("a" -> 1, "c" -> 10))
      .shouldEqual(Map("a" -> 1, "c" -> -10))

    parseInstructions(input).map(runFromEmptyLastMax).shouldEqual(Right(1))
  }

  it should "parse input as expected" in {

    parseRegister("b inc 5 if a > 1").map(_._1).shouldEqual(Some("b"))
    parseOperation("b inc 5 if a > 1").map(_._1).shouldEqual(Some(Inc("b", 5)))

    parseLine("b inc 5 if a > 1").map(_._1) shouldEqual (Some(Instruction(Inc("b", 5), Gt("a", 1))))

    input
      .split("\n")
      .flatMap(parseLine(_).map(_._1).toList)
      .shouldEqual(
        List(
          Instruction(Inc("b", 5), Gt("a", 1)),
          Instruction(Inc("a", 1), Lt("b", 5)),
          Instruction(Dec("c", -10), Gte("a", 1)),
          Instruction(Inc("c", -20), Equ("c", 10))))
  }
}
