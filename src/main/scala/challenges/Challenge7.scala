package com.eddsteel.advent17.challenges
import _root_.cats.Show
import _root_.cats.data.NonEmptyList
import _root_.cats.implicits._

// This got ugly fast :(
object Challenge7 extends Challenge {
  import Parser._
  import Parsers._

  type Name = String
  type Assn = (Name, Name)
  type Associations = List[Assn]

  final case class Program(name: Name, weight: Int)
  object Program {
    implicit val show: Show[Program] = cats.derive.show[Program]
  }

  sealed trait ProgramTree {
    def program: Program
    def children: List[ProgramTree]
    def totalWeight: Int

    def collectFirst[A](pf: PartialFunction[ProgramTree, A]): Option[A] = this match {
      case l: ProgramTree.ProgramLeaf => if (pf.isDefinedAt(l)) Some(pf(l)) else None
      case n: ProgramTree.ProgramNode =>
        n.children.collectFirst(pf).orElse {
          if (pf.isDefinedAt(n)) Some(pf(n)) else None
        }
    }
  }

  object ProgramTree {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def build(
      root: Program,
      nameAssociations: Map[String, NonEmptyList[String]],
      programs: Map[Name, Program]): ProgramTree =
      nameAssociations.get(root.name) match {
        case Some(names) =>
          ProgramNode(
            root,
            names.toList.flatMap(programs.get(_).toList).map(build(_, nameAssociations, programs)))
        case None =>
          ProgramLeaf(root)
      }

    implicit val show: Show[ProgramTree] = new Show[ProgramTree] {
      def show(pt: ProgramTree): String = pt match {
        case ProgramLeaf(program) =>
          program.show

        case ProgramNode(program, children) =>
          val childrenShow = children.map(c => s"\t${c.show}\n")
          s"\n\t${program.show}\n$childrenShow"
      }
    }

    final case class ProgramLeaf(program: Program) extends ProgramTree {
      def children = List.empty[ProgramTree]
      def totalWeight = program.weight
    }
    final case class ProgramNode(program: Program, children: List[ProgramTree])
        extends ProgramTree {
      def totalWeight = program.weight + children.map(_.totalWeight).sum
    }
  }

  // fwft (72) -> ktlj, cntj, xhth | fwft | (72) -> ktlj, cntj, xhth
  def parseName: Parser[Name] =
    parseAlphas

  // fwft (72) -> ktlj, cntj, xhth | Program(fwft,72) | -> ktlj, cntj, xhth
  // don't know what (nn) is yet.
  def parseProgram: Parser[Program] =
    (parseName <* parseChar(' ') <* parseChar('('), parseNumber <* parseChar(')')).mapN {
      case (n, number) => Program(n, number.toInt)
    }

  //  ktlj, cntj, xhth | NonEmptyList("ktlj", "cntj", "xhth") | ""
  def parseNameChildren: Parser[NonEmptyList[Name]] =
    parseOneOrMany(parseName <* parseOpt(parseString(", ")))

  // fwft (72) -> ktlj, cntj, xhth | Program(fwft,72), Map(fwft -> ktlj, fwft -> cntj, fwft -> xhth) | ""
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def parseLine: Parser[(Program, Associations)] =
    (parseProgram <* parseOpt(parseString(" -> ")), parseOpt(parseNameChildren)).mapN {
      case (program, children) =>
        program -> children.map(_.toList.map(program.name -> _)).getOrElse(List.empty[Assn])
    }

  def findRoot(associations: Map[Name, NonEmptyList[Name]]): Option[Name] = {
    val sources = associations.keySet
    val targets = associations.values.flatMap(_.toList).toSet

    sources.diff(targets).toList.headOption
  }

  def parseInput(input: String): ErrorOr[NonEmptyList[(Program, Associations)]] =
    input
      .split("\n")
      .toList
      .map(parseLine(_).map(_._1))
      .sequence
      .flatMap(NonEmptyList.fromList)
      .toRight("Fail.")

  def buildTree(input: NonEmptyList[(Program, Associations)]): ErrorOr[ProgramTree] = {
    val (programs, associations) = input.toList.separate
    val programIndex = programs.map(p => p.name -> p).toMap
    val allAssociations: Map[Name, NonEmptyList[Name]] =
      associations.combineAll.groupBy(_._1).mapValues(_.map(_._2)).collect {
        case (k, v :: vs) => k -> NonEmptyList(v, vs)
      }

    val root = findRoot(allAssociations).flatMap(programIndex.get)

    root.map(ProgramTree.build(_, allAssociations, programIndex)).toRight("Failed to build tree!")
  }

  def requiredImbalancedWeight(tree: ProgramTree): Option[(Name, Int)] = {
    def totalWeights(t: ProgramTree): List[(Int, List[ProgramTree])] =
      t.children.groupBy(_.totalWeight).toList

    def imbalanced(t: ProgramTree): Boolean = totalWeights(t).length === 2

    val imbalancedWeights: Option[List[(Int, List[Program])]] = tree.collectFirst {
      case t if imbalanced(t) =>
        totalWeights(t).map {
          case (w, trees) =>
            w -> trees.map(_.program)
        }
    }

    imbalancedWeights.flatMap { weights =>
      weights.sortBy(-_._2.length) match {
        case (w1, _) :: (w2, ns2) :: _ =>
          val change = w1 - w2
          ns2.headOption.map(prog => prog.name -> (prog.weight + change))
        case _ => None
      }
    }
  }

  def run(star: Star, input: String): String = {
    val calc: ProgramTree => String =
      star.switch(
        pt => pt.program.name.show,
        // this isn't right, but gave the right difference to apply to this answer's first child. Weird
        pt => pt.show |+| requiredImbalancedWeight(pt).show
      )

    parseInput(input).flatMap(buildTree).map(calc).show
  }
}
