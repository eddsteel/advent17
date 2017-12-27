package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

//
object Challenge12 extends Challenge {
  import Parser._, Parsers._

  type Association = Set[Int]
  type Associations = Vector[Association]

  def createAssociations(size: Int): Associations =
    Vector.fill(size)(Set.empty)

  def addAssociation(i: Int, connected: Association, associations: Associations): Associations = {
    val newAssociations = associations(i) |+| connected

    associations.updated(i, newAssociations)
  }

  def conns(associations: Associations, program: Int): Association = associations(program)

  @annotation.tailrec
  def deepConns(
    associations: Associations,
    queue: Set[Int],
    visited: Set[Int],
    collected: Association): Association = queue.toList match {
    case Nil => collected
    case next :: rest =>
      val cs = conns(associations, next)
      deepConns(
        associations,
        queue = (cs |+| rest.toSet).filterNot(visited.contains),
        visited |+| Set(next),
        collected |+| cs)
  }

  def parseAssociation: Parser[(Int, Association)] =
    for {
      i <- parseNumber <* parseString(" <-> ")
      as <- parseMany(parseNumber <* parseString(", "))
      a <- parseNumber
    } yield i.toInt -> (as :+ a).map(_.toInt).toSet

  // From a list of associations, find a group, if there are any left, and return the rest of the associations not in the group.
  def findGroup(a: Associations): (Option[Association], Associations) = {
    val nextStart = a.find { case vs => vs.nonEmpty }
    nextStart.map { start =>
      val group = deepConns(a, start, Set.empty, Set.empty)
      val filteredAssns = group.toList.foldRight(a) {
        case (next, state) => state.updated(next, Set.empty)
      }

      Some(group) -> filteredAssns
    }.getOrElse(None -> a)
  }

  @annotation.tailrec
  def findGroups(a: Associations, groups: List[Association]): List[Association] =
    findGroup(a) match {
      case (Some(group), assns) => findGroups(assns, group :: groups)
      case _                    => groups
    }

  def run(star: Star, input: String): String = {
    val parsed = parseMany(parseAssociation <* parseString("\n"))(input)
    val toAdd = parsed.map(_._1).toList.flatten
    val populated = toAdd.foldLeft(createAssociations(2000)) {
      case ((assns, (i, a))) => addAssociation(i, a, assns)
    }

    val result: Int = star.switch(
      deepConns(populated, Set(0), Set.empty, Set.empty).size,
      findGroups(populated, List.empty[Association]).size
    )

    result.show
  }

}
