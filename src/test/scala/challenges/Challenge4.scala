package com.eddsteel.advent17.challenges
import _root_.org.scalatest._

class Challenge4Test extends FlatSpec {
  import Challenge4.{run => _, _}
  "challenge 4" should "follow examples" in {
    assert(readPassphrase("aa bb cc dd ee").map(noDupes) === Some(true))
    assert(!(readPassphrase("aa bb cc dd aa").map(noDupes) === Some(true)))
    assert(readPassphrase("aa bb cc dd aaa").map(noDupes) === Some(true))

    assert(readPassphrase("abcde fghij").map(noAnagrams) === Some(true))
    assert(readPassphrase("abcde xyz ecdab").map(noAnagrams) === Some(false))
    assert(readPassphrase("a ab abc abd abf abj").map(noAnagrams) === Some(true))
    assert(readPassphrase("iiii oiii ooii oooi oooo").map(noAnagrams) === Some(true))
    assert(readPassphrase("oiii ioii iioi iiio").map(noAnagrams) === Some(false))
  }
}
