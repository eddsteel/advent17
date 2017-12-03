package com.eddsteel.advent17
package aoc
import types._
import _root_.cats.effect.IO
import _root_.cats.syntax.either._
import _root_.ciris._
import _root_.ciris.refined._
import _root_.eu.timepit.refined._
import _root_.eu.timepit.refined.types.string.NonEmptyString
import _root_.org.http4s._

final case class AdventOfCodeConfig(
  year: Year,
  baseUrl: Uri,
  cookie: eu.timepit.refined.types.string.NonEmptyString
)

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object AdventOfCodeConfig {
  def load: IO[AdventErrorOr[AdventOfCodeConfig]] = IO {
    loadConfig(
      env[NonEmptyString]("COOKIE")
    )(
      AdventOfCodeConfig(refineMV[Y](2017), Uri.uri("http://adventofcode.com"), _)
    ).leftMap(BadConfig.apply(_))
  }
}
