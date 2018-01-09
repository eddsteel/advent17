package com.eddsteel.advent17.challenges
import _root_.cats.implicits._
import _root_.cats._
import _root_.cats.data.NonEmptyList
import _root_.scala.util.matching.Regex

// Parser type is defined in package.scala

object Parser {
  implicit val parserInstances: Applicative[Parser] with Monad[Parser] =
    new Applicative[Parser] with Monad[Parser] {
      override def pure[A](x: A): Parser[A] = s => Some((x, s))

      override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
        s =>
          for {
            (f, t) <- ff(s)
            (a, u) <- fa(t)
          } yield (f(a) -> u)

      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
        s =>
          for {
            (a, t) <- fa(s)
            (b, u) <- f(a)(t)
          } yield (b, u)

      def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] =
        s =>
          f(a)(s) match {
            case Some((Right(b), s)) => Some((b, s))
            case _                   => None
        }
    }

  def pureOption[A](x: Option[A]): Parser[A] = s => x.map((_, s))

  def flattenParser[A](parser: Parser[Option[A]]): Parser[A] =
    str =>
      parser(str).flatMap {
        case (Some(a), s) => Some((a, s))
        case _            => None
    }

  implicit class ParserSyntax[A](p: Parser[A]) {
    def or(q: => Parser[A]): Parser[A] =
      s => p(s).orElse(q(s))
  }
}

object Parsers {
  import Parser._

  def parseRegex(re: Regex): Parser[String] =
    s =>
      s"^${re.toString}".r.findFirstIn(s).map { g =>
        (g, s.drop(g.length)) // TODO: is there a regex method for this?
    }

  // hella not stack safe
  def parseOneOrMany[A](parser: Parser[A]): Parser[NonEmptyList[A]] =
    s =>
      for {
        (hd, rest) <- parser(s)
        (tl, out) <- parseMany(parser)(rest)
      } yield (NonEmptyList(hd, tl) -> out)

  def parseMany[A](parser: Parser[A]): Parser[List[A]] =
    s => parseOneOrMany(parser).map(_.toList)(s).orElse(Some((List.empty[A], s)))

  def parseAnyChar: Parser[Char] =
    s =>
      s.headOption.map { h =>
        (h, s.drop(1))
    }

  def parseChar(c: Char): Parser[String] =
    parseRegex(s"[$c]".r)

  def parseString(str: String): Parser[String] =
    s =>
      if (s.startsWith(str)) Some((str, s.drop(str.length)))
      else None

  def parseOpt[A](parser: Parser[A]): Parser[Option[A]] =
    s => parser.map(Some.apply)(s).orElse(Some((None, s)))

  val parseAlpha: Parser[String] =
    parseRegex("[a-z]".r)

  val parseAlphas: Parser[String] =
    parseOneOrMany(parseAlpha).map(_.toList.mkString)

  val parseDigit: Parser[String] =
    parseRegex("[0-9]".r)

  val parseDigits: Parser[String] =
    parseOneOrMany(parseDigit).map(_.toList.mkString)

  // if the string isn't a valid int this will still consume it, but
  // numbers shouldn't so I guess were ok?
  val parseNumber: Parser[Long] =
    (
      parseOpt(parseChar('-')),
      parseDigits.flatMap(s => Parser.pureOption(Util.parseLong(s).toOption))).mapN {
      case (Some("-"), l) => -l
      case (_, l)         => l
    }
}
