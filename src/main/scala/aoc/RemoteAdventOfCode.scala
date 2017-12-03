package com.eddsteel.advent17
package aoc
import types._
import _root_.cats.effect._
import _root_.eu.timepit.refined.types.string.NonEmptyString
import _root_.org.http4s._
import _root_.org.http4s.client.blaze._
import _root_.org.http4s.client.dsl.Http4sClientDsl
import _root_.org.http4s.dsl.io._

class RemoteAdventOfCode(val year: Year, base: Uri, cookie: NonEmptyString)
    extends AdventOfCode
    with Http4sClientDsl[IO] {
  import RemoteAdventOfCode._

  def get(path: String): IO[String] = {
    val request = GET(base.withPath(path), headers = Header("Cookie", cookie.toString))

    for {
      resp <- httpClient.expect[String](request)
      _ <- IO(println(resp))
    } yield resp
  }

  def post(path: String, params: (String, String)*): IO[Unit] =
    // not working yet.
    IO(println(base.withPath(path)))
  /*    val request =
      POST(
        base.withPath(path),
        UrlForm(params: _*),
        headers = Seq(
          Header("Cookie", cookie.toString),
          Header("DNT", "1"),
          Header("Upgrade-Insecure-Requests", "1")): _*)

    for {
      resp <- httpClient.expect[String](request)
      _ <- IO(println(resp))
    } yield ()*/
}

object RemoteAdventOfCode {
  private val httpClient = PooledHttp1Client[IO]()

  def cleanUp(): Unit = httpClient.shutdownNow()
}
