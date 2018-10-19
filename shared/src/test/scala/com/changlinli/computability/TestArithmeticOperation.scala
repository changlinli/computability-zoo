package com.changlinli.computability

import org.scalatest.FlatSpec

class TestArithmeticOperation extends FlatSpec {
  "ArithmeticOperation.fromString" should "parse '1 + 1' correctly" in {
    val one = NumLiteral(NaturalNum.fromInt(1).get)
    assert(ArithmeticOperation.fromString("1 + 1") === Some(Plus(one, one)))
  }
  it should "parse '1 + 2 * 3' with the correct operator precedence" in {
    val one = NumLiteral(NaturalNum.fromInt(1).get)
    val two = NumLiteral(NaturalNum.fromInt(2).get)
    val three = NumLiteral(NaturalNum.fromInt(3).get)
    assert(ArithmeticOperation.fromString("1 + 2 * 3") === Some(Plus(one, Multiply(two, three))))
  }
  it should "parse '1 * 2 + 3' with the correct operator precedence" in {
    val one = NumLiteral(NaturalNum.fromInt(1).get)
    val two = NumLiteral(NaturalNum.fromInt(2).get)
    val three = NumLiteral(NaturalNum.fromInt(3).get)
    assert(ArithmeticOperation.fromString("1 * 2 + 3") === Some(Plus(Multiply(one, two), three)))
  }
  it should "parse '(1 + 2) * 3' with the correct operator precedence" in {
    val one = NumLiteral(NaturalNum.fromInt(1).get)
    val two = NumLiteral(NaturalNum.fromInt(2).get)
    val three = NumLiteral(NaturalNum.fromInt(3).get)
    assert(ArithmeticOperation.fromString("(1 + 2) * 3") === Some(Multiply(Plus(one, two), three)))
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

  "calculateFromString" should "calculate (1 + 2) * 3 as 9" in {
    val nineOpt = NaturalNum.fromInt(9)
    println(ArithmeticOperation.calculateFromString("(1 + 2) * 3").toList.flatMap(_.toList).map(LambdaTerm.prettyPrint).mkString("\n"))
    assert(ArithmeticOperation.calculateFromString("(1 + 2) * 3").flatMap(_.last.|>(LambdaTerm.convertToNaturalNum)) === nineOpt)
  }
}
