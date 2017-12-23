package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge12Test extends FlatSpec with Matchers with OptionValues {
  import Challenge12.{run => _, _}

  "Challenge 12" should "work like examples" in {
    val a0 = createAssociations(7)

    val a1 = addAssociation(0, Set(2), a0)
    val a2 = addAssociation(1, Set(1), a1)
    val a3 = addAssociation(2, Set(0, 3, 4), a2)
    val a4 = addAssociation(3, Set(2, 4), a3)
    val a5 = addAssociation(4, Set(2, 3, 6), a4)
    val a6 = addAssociation(5, Set(6), a5)
    val assns = addAssociation(6, Set(4, 5), a6)

    conns(assns, 3).shouldEqual(Set(2, 4))
    conns(assns, 0).shouldEqual(Set(2))

    deepConns(assns, Set(0), Set.empty, Set.empty).shouldEqual(Set(0, 2, 3, 4, 5, 6))

    findGroups(assns, List.empty).shouldEqual(List(Set(1), Set(0, 2, 3, 4, 5, 6)))

  }
}
