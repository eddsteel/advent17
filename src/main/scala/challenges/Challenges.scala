package com.eddsteel.advent17
package challenges
import types._

trait Challenge {
  def run(star: Star, input: String): String
}

object Challenge {
  val Unimplemented: Challenge =
    new Challenge() {
      def run(star: Star, input: String): String = "not implemented yet."
    }
}

object Challenges {
  def apply(d: Day): Challenge = d.value match {
    case 1  => Challenge1
    case 2  => Challenge2
    case 3  => Challenge3
    case 4  => Challenge4
    case 5  => Challenge5
    case 6  => Challenge6
    case 7  => Challenge7
    case 8  => Challenge8
    case 9  => Challenge9
    case 10 => Challenge10
    case 11 => Challenge11
    case 12 => Challenge12
    case 13 => Challenge13
    case 15 => Challenge15
    case _  => Challenge.Unimplemented
  }
}
