package com.eddsteel.advent17.challenges
import _root_.cats.implicits._
import _root_.cats.data.NonEmptyList
import _root_.org.scalatest._

class Challenge7Test extends FlatSpec with Matchers {
  import Challenge7.{run => _, _}

  val input = """pbga (66)
                |xhth (57)
                |ebii (61)
                |havc (66)
                |ktlj (57)
                |fwft (72) -> ktlj, cntj, xhth
                |qoyq (66)
                |padx (45) -> pbga, havc, qoyq
                |tknk (41) -> ugml, padx, fwft
                |jptl (61)
                |ugml (68) -> gyxo, ebii, jptl
                |gyxo (61)
                |cntj (57)""".stripMargin

  "challenge 7" should "follow examples" in {
    parseInput(input).flatMap(buildTree).map(_.program.name.show).shouldEqual(Right("tknk"))

  }

  it should "parse input as expected" in {

    parseLine("fwft (72) -> ktlj, cntj, xhth").shouldEqual(
      Some(((Program("fwft", 72), List("fwft" -> "ktlj", "fwft" -> "cntj", "fwft" -> "xhth")), "")))

    parseLine("pbga (66)").shouldEqual(
      Some(((Program("pbga", 66), List.empty[(String, String)]), "")))

    parseInput(input).shouldEqual(
      Right(NonEmptyList.of(
        Program("pbga", 66) -> Seq.empty[(String, String)],
        Program("xhth", 57) -> Seq.empty[(String, String)],
        Program("ebii", 61) -> Seq.empty[(String, String)],
        Program("havc", 66) -> Seq.empty[(String, String)],
        Program("ktlj", 57) -> Seq.empty[(String, String)],
        Program("fwft", 72) -> Seq("fwft" -> "ktlj", "fwft" -> "cntj", "fwft" -> "xhth"),
        Program("qoyq", 66) -> Seq.empty[(String, String)],
        Program("padx", 45) -> Seq("padx" -> "pbga", "padx" -> "havc", "padx" -> "qoyq"),
        Program("tknk", 41) -> Seq("tknk" -> "ugml", "tknk" -> "padx", "tknk" -> "fwft"),
        Program("jptl", 61) -> Seq.empty[(String, String)],
        Program("ugml", 68) -> Seq("ugml" -> "gyxo", "ugml" -> "ebii", "ugml" -> "jptl"),
        Program("gyxo", 61) -> Seq.empty[(String, String)],
        Program("cntj", 57) -> Seq.empty[(String, String)]
      )))

  }

  it should "calculate weight correctly" in {
    val tree = parseInput(input).flatMap(buildTree).right.get

    tree.children.map(_.totalWeight).shouldEqual(List(251, 243, 243))
    tree.totalWeight.shouldEqual(243 + 243 + 251 + 41)
  }

  it should "identify imbalanced tree correctly" in {
    val tree = parseInput(input).flatMap(buildTree).right.get
    requiredImbalancedWeight(tree).shouldEqual(Some(("ugml", 60)))
  }
}
