package com.changlinli.computability

import cats.Order
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}

sealed trait NaturalNum
case object Zero extends NaturalNum
final case class Succ(x: NaturalNum) extends NaturalNum

object NaturalNum {
  implicit val naturalNumArb: Arbitrary[NaturalNum] = Arbitrary{
    // The .get is safe because we're guaranteed that it's non-negative
    Gen.choose(0, Int.MaxValue).map(fromInt(_).get)
  }

  implicit val naturalNumIsOrdered: Order[NaturalNum] = new Order[NaturalNum] {
    override def compare(x: NaturalNum, y: NaturalNum): Int =
      Order[Int].compare(toInt(x), toInt(y))
  }

  def decrementOrFloor(x: NaturalNum): NaturalNum = x match {
    case Succ(prev) => prev
    case Zero => Zero
  }

  def plus(x: NaturalNum, y: NaturalNum): NaturalNum = x match {
    case Zero => y
    case Succ(prev) => Succ(plus(prev, y))
  }

  def increment(x: NaturalNum): NaturalNum = Succ(x)

  def zero: NaturalNum = Zero

  def fromInt(int: Int): Option[NaturalNum] = {
    if (int < 0) {
      None
    } else if (int === 0) {
      Some(Zero)
    } else {
      fromInt(int - 1).map(Succ.apply)
    }
  }

  def toInt(naturalNum: NaturalNum): Int = naturalNum match {
    case Zero => 0
    case Succ(previous) => toInt(previous) + 1
  }
}

