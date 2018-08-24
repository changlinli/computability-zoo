package com.changlinli.computability

sealed trait ArithmeticOperation
final case class Plus(x: NaturalNum, y: NaturalNum) extends ArithmeticOperation
final case class Monus(x: NaturalNum, y: NaturalNum) extends ArithmeticOperation
final case class Multiply(x: NaturalNum, y: NaturalNum) extends ArithmeticOperation
final case class FloorDivision(x: NaturalNum, y: NaturalNum) extends ArithmeticOperation

