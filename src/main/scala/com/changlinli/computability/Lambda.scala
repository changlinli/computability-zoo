package com.changlinli.computability

import cats.Eval

object Lambda {

  val identityLambda: LambdaTerm = Abstraction(Variable("x"), Variable("x"))

  def fromNaturalNum(num: NaturalNum): LambdaTerm =
    Abstraction(Variable("f"), Abstraction(Variable("x"), fromNaturalNumCompositionTerm(num)))

  private def fromNaturalNumCompositionTerm(num: NaturalNum): LambdaTerm = num match {
    case Zero =>
      Variable("x")
    case Succ(prev) =>
      Application(Variable("f"), fromNaturalNumCompositionTerm(prev))
  }

  def parse(input: String): Option[LambdaTerm] = ???

  // lambda f. lambda x. x
  val zero: LambdaTerm = Abstraction(Variable("f"), Abstraction(Variable("x"), Variable("x")))

  def retrieveFreeVariables(lambdaTerm: LambdaTerm): Set[Variable] =
    retrieveFreeVariablesRec(lambdaTerm, Set.empty)

  def retrieveFreeVariablesRec(lambdaTerm: LambdaTerm, currentSet: Set[Variable]): Set[Variable] = lambdaTerm match {
    case variable @ Variable(_) =>
      currentSet + variable
    case Abstraction(variable, term) =>
      retrieveFreeVariablesRec(term, currentSet - variable)
    case Application(applier, applied) =>
      retrieveFreeVariablesRec(applier, currentSet) ++ retrieveFreeVariablesRec(applied, currentSet)
  }
}
