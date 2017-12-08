package com.eddsteel.advent17.challenges
import _root_.cats.data.NonEmptyList
import _root_.cats.implicits._

object Challenge4 extends Challenge {
  type Line = String
  type Word = String
  type Passphrase = NonEmptyList[Word]

  def readPassphrase(line: Line): Option[Passphrase] =
    NonEmptyList.fromList(line.split(" ").toList)

  def noDupes(pass: Passphrase): Boolean =
    pass.distinct === pass

  def noAnagrams(pass: Passphrase): Boolean =
    noDupes(pass.map(_.sorted))

  def run(star: Star, input: String): String =
    input
      .split("\n")
      .flatMap(readPassphrase(_).toList)
      .filter(star.switch(noDupes _, noAnagrams _))
      .size
      .show
}
