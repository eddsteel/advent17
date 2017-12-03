package com.eddsteel.advent17
package aoc
import types._
import _root_.cats.effect.IO
import _root_.cats.implicits._
import _root_.eu.timepit.refined.cats._
import _root_.net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import _root_.net.ruippeixotog.scalascraper.dsl.DSL._
import _root_.net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import _root_.org.apache.commons.lang3.text.WordUtils

trait AdventOfCode {
  def year: Year

  def getDescription(day: Day): IO[String] =
    get(s"/${year.show}/day/${day.show}").flatMap { html =>
      val doc = AdventOfCode.browser.parseString(html)
      val title = doc >> text("main article h2")
      val body = (doc >> elementList("main article p, main article ul, main article pre"))
        .map(_ >> text("*"))
      val description =
        s"${title}\n\n${WordUtils.wrap(body.map(_.replaceAll("\n", "")).mkString("\n\n"), 80)}"

      IO(description)
    }

  def getInput(day: Day): IO[String] =
    get(s"/${year.show}/day/${day.show}/input").flatMap { res =>
      IO(println(res)).map(_ => res)
    }

  def postSolution(day: Day, solution: String): IO[Unit] =
    post(s"/${year.show}/day/${day.show}/answer", "answer" -> solution, "level" -> day.show)

  def get(path: String): IO[String]

  def post(path: String, params: (String, String)*): IO[Unit]
}

object AdventOfCode {
  val browser: Browser = new JsoupBrowser

  def apply(config: AdventOfCodeConfig): AdventOfCode =
    new PassThroughAdventOfCode(new RemoteAdventOfCode(config.year, config.baseUrl, config.cookie))
}
