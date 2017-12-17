package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge9Test extends FlatSpec with Matchers with OptionValues {
  import Challenge9.{run => _, _}

  def parserValue[A](o: Option[(A, String)]): A = o.value._1

  "Challenge9" should "recognise garbage" in {
    parserValue(parseGarbage("<>")).shouldEqual(Garbage("<>", ""))
    parserValue(parseGarbage("<random characters>"))
      .shouldEqual(Garbage("<random characters>", "random characters"))
    parserValue(parseGarbage("<<<<>")).shouldEqual(Garbage("<<<<>", "<<<"))
    parserValue(parseGarbage("<{!>}>")).shouldEqual(Garbage("<{!>}>", "{}"))
    parserValue(parseGarbage("<!!>")).shouldEqual(Garbage("<!!>", ""))
    parserValue(parseGarbage("<!!!>>")).shouldEqual(Garbage("<!!!>>", ""))
    parserValue(parseGarbage("<{o\"i!a,<{i<a>"))
      .shouldEqual(Garbage("<{o\"i!a,<{i<a>", "{o\"i,<{i<a"))
  }

  it should "recognise groups" in {
    parseGroup("kjlkj").shouldEqual(None)
    parserValue(parseGroup("{}")).shouldEqual(Group(List.empty, 1))
    parserValue(parseGroup("{{{}}}"))
      .shouldEqual(Group(List(Group(List(Group(List.empty, 1)), 2)), 3))
    parserValue(parseGroup("{{},{}}"))
      .shouldEqual(Group(List(Group(List.empty, 1), Group(List.empty, 1)), 3))
    parserValue(parseGroup("{{{},{},{{}}}}")).shouldEqual(
      Group(
        List(
          Group(
            List(Group(List.empty, 1), Group(List.empty, 1), Group(List(Group(List.empty, 1)), 2)),
            5)),
        6))
  }

  it should "recognise mixed content" in {
    parserValue(parseGroup("{<{},{},{{}}>}"))
      .shouldEqual(Group(List(Garbage("<{},{},{{}}>", "{},{},{{}}")), 1))
    parserValue(parseGroup("{<a>,<a>,<a>,<a>}")).shouldEqual(
      Group(
        List(Garbage("<a>", "a"), Garbage("<a>", "a"), Garbage("<a>", "a"), Garbage("<a>", "a")),
        1))
    parserValue(parseGroup("{{<a>},{<a>},{<a>},{<a>}}")).shouldEqual(
      Group(
        List(
          Group(List(Garbage("<a>", "a")), 1),
          Group(List(Garbage("<a>", "a")), 1),
          Group(List(Garbage("<a>", "a")), 1),
          Group(List(Garbage("<a>", "a")), 1)),
        5))
    parserValue(parseGroup("{{<!>},{<!>},{<!>},{<a>}}")).shouldEqual(
      Group(
        List(Group(List(Garbage("<!>},{<!>},{<!>},{<a>", "},{<},{<},{<a")), 1)),
        2
      )
    )
  }

  it should "count groups correctly" in {
    readGroup("{}").map(count).shouldEqual(Right(1))
    readGroup("{{{}}}").map(count).shouldEqual(Right(3))
    readGroup("{}").map(count).shouldEqual(Right(1))
    readGroup("{{},{}}").map(count).shouldEqual(Right(3))
    readGroup("{{{},{},{{}}}}").map(count).shouldEqual(Right(6))
    readGroup("{<{},{},{{}}>}").map(count).shouldEqual(Right(1))
    readGroup("{<a>,<a>,<a>,<a>}").map(count).shouldEqual(Right(1))
    readGroup("{{<!>},{<!>},{<!>},{<a>}}").map(count).shouldEqual(Right(2))
  }

  it should "score groups correctly" in {
    readGroup("{}").map(score).shouldEqual(Right(1))
    readGroup("{{{}}}").map(score).shouldEqual(Right(6))
    readGroup("{{},{}}").map(score).shouldEqual(Right(5))
    readGroup("{{{},{},{{}}}}").map(score).shouldEqual(Right(16))
    readGroup("{{<ab>},{<ab>},{<ab>},{<ab>}}").map(score).shouldEqual(Right(9))
    readGroup("{{<!!>},{<!!>},{<!!>},{<!!>}}").map(score).shouldEqual(Right(9))
    readGroup("{{<a!>},{<a!>},{<a!>},{<ab>}}").map(score).shouldEqual(Right(3))
  }

  private def readGarbage(in: String) = parseGarbage(in) match {
    case Some((grbg, _)) => Some(grbg)
    case _               => None
  }

  it should "count non-canceled chars correctly" in {
    readGarbage("<>").map(counted).value.shouldEqual(0)
    readGarbage("<random characters>").map(counted).value.shouldEqual(17)
    readGarbage("<<<<>").map(counted).value.shouldEqual(3)
    readGarbage("<{!>}>").map(counted).value.shouldEqual(2)
    readGarbage("<!!>").map(counted).value.shouldEqual(0)
    readGarbage("<!!!>>").map(counted).value.shouldEqual(0)
    readGarbage("<{o\"i!a,<{i<a>").map(counted).value.shouldEqual(10)

  }
}
