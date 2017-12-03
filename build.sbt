// scalafmt: {align.tokens = ["%", "%%", ":=", "+="]}
name         := "advent17"
description  := "Advent of Code 2017: typelevel scala"
scalaVersion := "2.12.4"
organization := "com.eddsteel"
version      := "day0"

licenses := List(
  ("GPL version 3", url("https://www.gnu.org/licenses/gpl-3.0.en.html")))
homepage := Some(url("https://github.com/eddsteel/advent17"))
developers := List(
  Developer("eddsteel",
            "Edd Steel",
            "edward.steel@gmail.com",
            url("https://github.com/eddsteel/advent17")))
scmInfo := Some(
  ScmInfo(url("https://github.com/eddsteel/advent17"),
          "scm:git:https://github.com/eddsteel/advent17.git"))

libraryDependencies += "org.typelevel"      %% "cats-core"           % "1.0.0-RC1"
libraryDependencies += "eu.timepit"         %% "refined"             % "0.8.4"
libraryDependencies += "org.scalatest"      %% "scalatest"           % "3.0.4" % "test"
libraryDependencies += "org.http4s"         %% "http4s-dsl"          % "0.18.0-M5"
libraryDependencies += "org.http4s"         %% "http4s-blaze-client" % "0.18.0-M5"
libraryDependencies += "com.monovore"       %% "decline"             % "0.4.0-RC1"
libraryDependencies += "com.monovore"       %% "decline-refined"     % "0.4.0-RC1"
libraryDependencies += "org.typelevel"      %% "kittens"             % "1.0.0-RC1"
libraryDependencies += "net.ruippeixotog"   %% "scala-scraper"       % "2.0.0"
libraryDependencies += "org.apache.commons" % "commons-lang3"        % "3.7"
libraryDependencies += "is.cir"             %% "ciris-core"          % "0.5.0"
libraryDependencies += "is.cir"             %% "ciris-refined"       % "0.5.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

Lint.settings
Flags.settings
Format.settings
