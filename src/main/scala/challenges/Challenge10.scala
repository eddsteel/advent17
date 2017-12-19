package com.eddsteel.advent17.challenges
import _root_.cats.Show
import _root_.cats.data.NonEmptyList
import _root_.cats.derive
import _root_.cats.implicits._

//
object Challenge10 extends Challenge {
//  import Parser._, Parsers._

  final case class CircularList[+A](elements: NonEmptyList[A]) {
    def sublist(start: Int, length: Int): NonEmptyList[A] = {
      require(start >= 0 && start < elements.length && length > 0 && length <= elements.length)

      NonEmptyList.fromListUnsafe(
        (elements.toList.drop(start) ::: elements.toList.take(start)).take(length))
    }

    def replaceSublist[B >: A](start: Int, list: NonEmptyList[B]): CircularList[B] = {
      require(start >= 0 && start < elements.length && list.length <= elements.length)

      val overlap = (start + list.length - elements.length).max(0)
      val sublist2 = elements.toList.slice(overlap, start)
      val (sublist3, sublist1) = list.toList.splitAt(list.length - overlap)
      val sublist4 = elements.toList.drop(start + list.length)

      CircularList(NonEmptyList.fromListUnsafe(sublist1 |+| sublist2 |+| sublist3 |+| sublist4))
    }

    def reverseSublist(start: Int, length: Int): CircularList[A] =
      if (length <= 1) this
      else replaceSublist(start, NonEmptyList.fromListUnsafe(sublist(start, length).toList.reverse))
  }

  final case class State(list: CircularList[Int], pos: Int, skip: Int)
  object State {
    @SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Nothing"))
    implicit val showState: Show[State] = derive.show[State]
  }

  def startState(list: NonEmptyList[Int]): State =
    State(CircularList(list), pos = 0, skip = 0)

  def twist(state: State, length: Int): State = {
    val newList = state.list.reverseSublist(state.pos, length)
    val newPos = (state.pos + length + state.skip) % newList.elements.length
    val newSkip = state.skip + 1

    State(newList, newPos, newSkip)
  }

  def runLengths(lengths: List[Int], upTo: Int): State =
    lengths.foldLeft(startState(NonEmptyList(0, Range(1, upTo).toList)))(twist)

  def multiplyList[A](in: List[A], times: Int): List[A] =
    Stream.continually(in).take(times).toList.flatten

  def xorAll(list: NonEmptyList[Int]): Int =
    list.tail.foldLeft(list.head)(_ ^ _)

  def asciiWithStdSuffix(in: String): NonEmptyList[Int] =
    NonEmptyList.fromListUnsafe(in.toList.map(_.toInt) |+| List(17, 31, 73, 47, 23))

  // going on faith in input for a lot of these NELs
  def sparseHashToDense(sparse: NonEmptyList[Int]): NonEmptyList[Int] =
    NonEmptyList.fromListUnsafe(
      sparse.toList.grouped(16).map(NonEmptyList.fromListUnsafe).map(xorAll).toList)

  def hexString(in: List[Int]): String =
    in.map(i => ("0" |+| Integer.toHexString(i)).takeRight(2)).mkString("")

  def solve1(result: CircularList[Int]): ErrorOr[Int] =
    Either.catchNonFatal {
      val List(a, b, _*) = result.elements.toList
      a * b
    }.leftMap(_ => s"Failed to solve $result")

  def run1(in: String): String =
    (for {
      lengths <- Util.parseInts(in.split(",").toList)
      finalState = runLengths(lengths, 256)
      out <- solve1(finalState.list)
    } yield out).fold(identity, _.show)

  def run2(in: String): String = {
    val lengths = multiplyList(asciiWithStdSuffix(in).toList, 64)
    val out = runLengths(lengths, 256)
    hexString(sparseHashToDense(out.list.elements).toList)
  }

  def run(star: Star, input: String): String =
    star.switch(run1 _, run2 _)(input)
}
