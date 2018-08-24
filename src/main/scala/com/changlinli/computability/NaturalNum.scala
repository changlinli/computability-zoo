package com.changlinli.computability

import cats.Order
import cats.implicits._

sealed trait NaturalNum
case object Zero extends NaturalNum
final case class Succ(x: NaturalNum) extends NaturalNum

object NaturalNum {
  implicit val naturalNumIsOrdered: Order[NaturalNum] = new Order[NaturalNum] {
    override def compare(x: NaturalNum, y: NaturalNum): Int =
      Order[Int].compare(toInt(x), toInt(y))
  }

  def increment(x: NaturalNum): NaturalNum = Succ(x)

  def zero: NaturalNum = Zero

  def fromInt(int: Int): Option[NaturalNum] = {
    if (int < 0) {
      None
    } else if (int === 0) {
      Some(Zero)
    } else {
      fromInt(int).map(Succ.apply)
    }
  }

  def toInt(naturalNum: NaturalNum): Int = naturalNum match {
    case Zero => 0
    case Succ(previous) => toInt(previous) + 1
  }
}
