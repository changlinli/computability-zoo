package com.changlinli.computability

import org.scalatest.FlatSpec

class TestArithmeticOperation extends FlatSpec {
  "ArithmeticOperation.fromString" should "parse '1 + 1' correctly" in {
    val one = NumLiteral(NaturalNum.fromInt(1).get)
    assert(ArithmeticOperation.fromString("1 + 1") === Some(Plus(one, one)))
  }

  "The ArithmeticOperation representing 1 + 1" should "calculate as 2 via the lambda calculus" in {
    val one = NumLiteral(NaturalNum.fromInt(1).get)
    val calculationResult = ArithmeticOperation.calculateThroughLambdaTerm(Plus(one, one))
      .last
      .|>(LambdaTerm.convertToNaturalNum)
    assert(calculationResult === Some(Succ(Succ(Zero))))
  }

  "The ArithmeticOperation representing 3 * 2" should "calculate as 6 via the lambda calculus" in {
    val three = NumLiteral(NaturalNum.fromInt(3).get)
    val two = NumLiteral(NaturalNum.fromInt(2).get)
    val calculationResult = ArithmeticOperation.calculateThroughLambdaTerm(Multiply(three, two))
      .last
      .|>(LambdaTerm.convertToNaturalNum)
    assert(calculationResult === Some(Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))))
  }
}
