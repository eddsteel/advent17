package com.eddsteel.advent17
package challenges
import _root_.cats.Eq
import _root_.cats.derive.{eq => deriveEq}
import _root_.cats.implicits._
import _root_.com.monovore.decline.Argument

sealed trait Star {
  def switch[A](one: => A, two: => A): A = this match {
    case Star1 => one
    case Star2 => two
  }
}
case object Star1 extends Star
case object Star2 extends Star

object Star {

  @SuppressWarnings(Array("org.wartremover.warts.Equals")) // this is wack
  implicit val eqStar: Eq[Star] = deriveEq[Star]

  implicit val arg: Argument[Star] = new Argument[Star]() {

    def defaultMetavar: String = "star"

    def read(string: String) =
      (string match {
        case "1" => Star1.valid[String]
        case "2" => Star2.valid[String]
        case x   => s"$x is not a valid star. Use '1' or '2'".invalid[Star]
      }).toValidatedNel
  }
}
