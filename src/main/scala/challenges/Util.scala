package com.eddsteel.advent17.challenges
import _root_.cats.Traverse
import _root_.cats.implicits._

object Util {

  def parseLong(s: String): ErrorOr[Long] =
    Either.catchNonFatal(s.toLong).leftMap(c => s"Bad input: $c")

  def parseInt(s: String): ErrorOr[Int] =
    Either.catchNonFatal(s.toInt).leftMap(c => s"Bad input: $c")

  def parseInts[T[_]: Traverse](s: T[String]): ErrorOr[T[Int]] =
    s.map(parseInt).sequence[ErrorOr, Int]

  def padStringL(length: Int, char: Char)(initial: String): String =
    initial.toString.reverse.padTo(length, char).reverse.mkString

}
