package com.eddsteel.advent17.challenges
import _root_.cats.Monoid
import _root_.cats.implicits._

//
object Challenge9 extends Challenge {
  import Parser._, Parsers._

  sealed trait StreamContent { def score: Int = 0 }
  final case class Garbage(original: String, counted: String) extends StreamContent
  final case class Group(content: List[StreamContent], override val score: Int)
      extends StreamContent

  object Garbage {
    implicit val garbageMonoid: Monoid[Garbage] = new Monoid[Garbage] {
      def empty: Garbage = Garbage("", "")
      def combine(a: Garbage, b: Garbage) =
        Garbage(a.original |+| b.original, a.counted |+| b.counted)
    }
  }

  // garbage goes up to a closer, but can be escaped with !
  def parseGarbageContent: Parser[(String, String)] =
    parseMany(
      parseRegex("!.".r).map(s => (s, "")).or(parseRegex("[^>]".r).map(s => (s, s)))
    ).map(_.combineAll)

  def parseGarbage: Parser[StreamContent] =
    (parseChar('<') *> parseGarbageContent <* parseChar('>')).map {
      case (allContent, nonGarbage) => Garbage(s"<$allContent>", nonGarbage)
    }

  def parseStreamContent: Parser[StreamContent] =
    parseGarbage.or(parseGroup)

  def parseGroupContent: Parser[List[StreamContent]] =
    parseMany(parseStreamContent <* parseOpt(parseChar(',')))

  def parseGroup: Parser[StreamContent] =
    (parseChar('{') *> parseGroupContent <* parseChar('}')).map { streamContent =>
      Group(streamContent, streamContent.map(_.score).combineAll + 1)
    }

  def readGroup(in: String): ErrorOr[Group] =
    Either.fromOption(parseGroup(in).collect {
      case (group: Group, _) => group
    }, "Could not parse input!")

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def flattenGroups(s: StreamContent): List[StreamContent] = s match {
    case g @ Group(content, _) => g :: content.flatMap(flattenGroups)
    case _                     => List.empty[StreamContent]
  }

  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  def flattenGarbage(s: StreamContent): List[Garbage] = s match {
    case g: Garbage        => List(g)
    case Group(content, _) => content.flatMap(flattenGarbage)
  }

  def count(in: StreamContent): Int = flattenGroups(in).length
  def score(in: StreamContent): Int = flattenGroups(in).foldMap(_.score)
  def counted(in: StreamContent): Int = flattenGarbage(in).combineAll.counted.length

  def run(star: Star, input: String): String = {
    val op = star.switch(score _, counted _)
    readGroup(input).map(op).fold(identity, _.show)
  }

}
