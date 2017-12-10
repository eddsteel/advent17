package com.eddsteel.advent17.challenges
import _root_.cats.implicits._

/* Second go at challenge three, solving this with maths. Incompatible with second star.
 */
object Challenge3a extends Challenge {
  val East: Int = -3
  val North: Int = -1
  val West: Int = 1
  val South: Int = 3

  val ordinals: Seq[Int] = Seq(East, North, West, South)

  // n | 4n^2 + On + 1
  def ordinalAtN(n: Int, direction: Int): Int =
    4 * n * n + direction * n + 1

  // if on an ordinal, i.e. nth in sequence, block distance is just n,
  // quadratic solver on 4n^2 + direction * n + 1 - number
  def distanceOrdinalD(number: Int, direction: Int): Double =
    (0 - direction + Math.sqrt(((direction * direction) - (16 * (1 - number)).toDouble))) / 8.0d

  // find closest ordinal for a number on the spiral.
  // try each direction and return closest, and the closest integral n
  //
  def closestOrdinal(number: Int): OrdinalProximity =
    ordinals.map(OrdinalProximity.build(_, number)).min

  final case class OrdinalProximity(ordinal: Int, distanceToOrdinal: Int)
  object OrdinalProximity {
    implicit val ordering: Ordering[OrdinalProximity] = Ordering.by(_.distanceToOrdinal)

    // build proximity info based on number we're looking for and distance we're trying.
    def build(ordinal: Int, target: Int): OrdinalProximity = {
      val distance = distanceOrdinalD(target, ordinal)
      val roundedN = distance.round.toInt
      val distanceToOrdinal = Math.abs(ordinalAtN(roundedN, ordinal) - target)

      new OrdinalProximity(ordinal, distanceToOrdinal)
    }
  }

  // Block distance alg.
  //
  // BD for m: Find closest ordinal o_n.
  // BD is diff(o_n, m) + n
  //
  def blockDistance(number: Int): Int =
    if (number === 1) 0
    else {
      val OrdinalProximity(ordinal, distance) = closestOrdinal(number)
      distanceOrdinalD(number, ordinal).toInt + distance
    }

  def run(star: Star, input: String): String = {
    val solver = star.switch(blockDistance _, blockDistance _)

    Util.parseInt(input).map(solver).fold(identity, _.show)
  }
}

/*
36  35  34  33  32  31
17  16  15  14  13  30
18   5   4   3  12  29
19   6   1   2  11  28
20   7   8   9  10  27
21  22  23  24  25  26


E from 1: 1 +1 2 +9  11    =
N from 1: 1 +3 4 +11 15    =
W from 1: 1 +5 6 +13 19    =
S from 1: 1 +7 8 +15 23    =

NE from 1: 1 +2 3 +10 13 +18: 31
NW from 1: 1 +4 5 +12 17
SW from 1: 1 +6 7 +14 21
SE from 1: 1 +8 9 +16 25

ordinals are
 o_0 = 1; o_n+1 = o_n + O + 8n

(looked up) general solution
 f(n) = 4n^2 + On + 1

n=0: 1
n=1: 5  + O
n=2: 17 + O2

O=1 :  E
O=2 : NE
O=3 :  N
O=4 : NW
O=5 :  W
O=6 : SW
O=7 :  S
O=8 : SE

 */
