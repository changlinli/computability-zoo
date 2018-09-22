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
      .flatMap(x => many(horizontalWhitespace) *> x.pure[Parser])

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

  private def chainLeft1[A](elemParser: Parser[A], operatorParser: => Parser[(A, A) => A]): Parser[A] = {
    def rest(z: A): Parser[A] =
      (for {
        operator <- operatorParser
        elem <- elemParser
        result <- rest(operator(z, elem))
      } yield result) | z.pure[Parser]
    elemParser.flatMap(rest)
  }

  def additionMultiTerm: Parser[ArithmeticOperation] = chainLeft1(multiTerm, parseAdd)

  def multiTerm: Parser[ArithmeticOperation] = chainLeft1(parseAtomic.covary[ArithmeticOperation], parseMulti)

  def parseAtomic: Parser[ArithmeticOperation] = myParens(additionMultiTerm) | parseNumLiteral.covary[ArithmeticOperation]

  def myParens[A](p: => Parser[A]): Parser[A] =
    bracket(char('('), p, char(')')) <~ many(horizontalWhitespace)

  def parseAdd: Parser[(ArithmeticOperation, ArithmeticOperation) => ArithmeticOperation] =
    for {
      _ <- char('+')
      _ <- many(horizontalWhitespace)
    } yield Plus(_, _)

  def parseMulti: Parser[(ArithmeticOperation, ArithmeticOperation) => ArithmeticOperation] =
    for {
      _ <- char('*')
      _ <- many(horizontalWhitespace)
    } yield Multiply(_, _)

  def calculateThroughLambdaTerm(x: ArithmeticOperation): NonEmptyList[LambdaTerm] =
    LambdaTerm.fromArithmeticOperationWithAutomaticNames(x)
      .|>(LambdaTerm.betaReduceScan)

  def fromString(str: String): Option[ArithmeticOperation] = {
    additionMultiTerm.parseOnly(str) match {
      case ParseResult.Done(_, result) =>
        Some(result)
      case ParseResult.Partial(_) =>
        throw new Exception("This should be impossible because we parseOnly-ed instead of parse-ing")
      case ParseResult.Fail(_, _, _) =>
        None
    }
  }

  def calculateFromString(str: String): Option[NonEmptyList[LambdaTerm]] =
    fromString(str).map(calculateThroughLambdaTerm)
}
