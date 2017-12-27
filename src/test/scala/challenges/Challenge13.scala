package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge13Test extends FlatSpec with Matchers with OptionValues {
  import Challenge13.{run => _, _}

  val examples =
    List(Layer(0, 3), Layer(1, 2), Layer(4, 4), Layer(6, 4))

  "Challenge 13" should "parse example layers" in {
    List("0: 3", "1: 2", "4: 4", "6: 4")
      .flatMap(s => parseLayer(s).toList.map(_._1))
      .shouldEqual(examples)
  }

  it should "work like examples" in {
    val List(layer0, layer1, layer4, layer6) = examples
    layer0.scannerAtTopWhenVisited.shouldEqual(true)
    layer1.scannerAtTopWhenVisited.shouldEqual(false)
    layer4.scannerAtTopWhenVisited.shouldEqual(false)
    layer6.scannerAtTopWhenVisited.shouldEqual(true)

    tripSeverity(examples).shouldEqual(24)

    layer0.scannerAtTopWithDelay(4).shouldEqual(true)
    layer1.scannerAtTopWithDelay(4).shouldEqual(false)
    layer4.scannerAtTopWithDelay(4).shouldEqual(false)
    layer6.scannerAtTopWithDelay(4).shouldEqual(false)

    layer0.scannerAtTopWithDelay(10).shouldEqual(false)
    layer1.scannerAtTopWithDelay(10).shouldEqual(false)
    layer4.scannerAtTopWithDelay(10).shouldEqual(false)
    layer6.scannerAtTopWithDelay(10).shouldEqual(false)

    assert(tripSeverityWithDelay(4, examples) === Some(0))
    assert(tripSeverityWithDelay(10, examples) === None)

    findDelay(examples).shouldEqual(10)

  }

}
