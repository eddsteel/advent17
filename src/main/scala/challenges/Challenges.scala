package com.eddsteel.advent17
package challenges
import types._

trait Challenge {
  def run(star: Star, input: String): String
}

object Challenges {
  def apply(d: Day): Challenge = d.value match {
    case 1 => Challenge1
    case 2 => Challenge2
    case 3 => Challenge3
    case 4 => Challenge4
    case 5 => Challenge5
    case _ =>
      new Challenge() {
        def run(star: Star, input: String): String = "not implemented yet."
      }
  }
}
