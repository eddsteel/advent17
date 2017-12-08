package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

object Challenge5 extends Challenge {

  type Position = Int
  type Offset = Int
  type Instructions = Vector[Offset]

  def jump(
    position: Position,
    state: Instructions,
    update: (Offset => Int)): (Position, Instructions) = {
    val current = state(position)
    val newPosition = current + position
    val newValue = current + update(current)

    newPosition -> state.updated(position, newValue)
  }

  def escaped(position: Position, length: Int): Boolean =
    0 > position || position >= length

  def runToCompletion(step: Int => Int)(
    instructions: Instructions): Option[(Long, (Position, Instructions), Boolean)] = {
    val length = instructions.length
    val states = Stream.iterate((0l, (0, instructions), false)) {
      case (count, state, true) => (count, state, true)
      case (count, (p, v), false) =>
        val (pos, vec) = jump(p, v, step)
        val next = (count + 1, (pos, vec), escaped(pos, length))
        next
    }

    states.find(_._3)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.While"))
  def runToCompletionFast(step: Int => Int)(
    instructions: Instructions): Option[(Long, (Position, Instructions), Boolean)] = {
    val length = instructions.length
    // I haven't written code like this in like 6 years
    var finished = false
    var count = 0l
    var pos = 0
    var state = instructions
    while (!finished) {
      val (p, s) = jump(pos, state, step)
      pos = p
      state = s
      count = count + 1
      finished = escaped(pos, length)
    }

    Some((count, (pos, state), finished))
  }

  def run(star: Star, input: String): String = {
    val instructions =
      input.split("\n").toVector.map(_.toInt)

    val step = star.switch({ _: Int =>
      1
    }, { o: Int =>
      if (o >= 3) -1 else 1
    })

    runToCompletionFast(step)(instructions) match {
      case Some((count, _, _)) => count.show
      case _                   => "Failzo"
    }
  }
}
