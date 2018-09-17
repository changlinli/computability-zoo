package com.changlinli.computability

import atto._
import Atto._
import cats.data.NonEmptyList
import cats.implicits._

sealed trait ArithmeticOperation
final case class NumLiteral(x: NaturalNum) extends ArithmeticOperation
final case class Plus(x: ArithmeticOperation, y: ArithmeticOperation) extends ArithmeticOperation
final case class Multiply(x: ArithmeticOperation, y: ArithmeticOperation) extends ArithmeticOperation

object ArithmeticOperation {
  val parseNumLiteral: Parser[NumLiteral] =
    int
      .map(NaturalNum.fromInt)
      .collect{case Some(x) => x}
      .map(NumLiteral.apply)

  def parsePlus: Parser[Plus] = for {
    x <- parseNumLiteral
    _ <- many(horizontalWhitespace)
    _ <- char('+')
    _ <- many(horizontalWhitespace)
    y <- parseNumLiteral
  } yield Plus(x, y)

  def parseMultiply: Parser[Multiply] = for {
    x <- parseNumLiteral
    _ <- many(horizontalWhitespace)
    _ <- char('*')
    _ <- many(horizontalWhitespace)
    y <- parseNumLiteral
  } yield Multiply(x, y)

  def calculateThroughLambdaTerm(x: ArithmeticOperation): NonEmptyList[LambdaTerm] =
    LambdaTerm.fromArithmeticOperationWithAutomaticNames(x)
      .|>(LambdaTerm.betaReduceScan)

  def parseArithmeticOperation: Parser[ArithmeticOperation] =
    parsePlus.covary[ArithmeticOperation]
      .|(parseMultiply.covary[ArithmeticOperation])
      .|(parseNumLiteral.covary[ArithmeticOperation])

  def fromString(str: String): Option[ArithmeticOperation] = {
    parseArithmeticOperation.parseOnly(str) match {
      case ParseResult.Done(_, result) =>
        Some(result)
      case ParseResult.Partial(_) =>
        throw new Exception("This should be impossible because we parseOnly-ed insead of parse-ing")
      case ParseResult.Fail(_, _, _) =>
        None
    }
  }
}
