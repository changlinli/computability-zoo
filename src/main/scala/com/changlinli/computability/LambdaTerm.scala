package com.changlinli.computability

sealed trait LambdaTerm
final case class Variable(name: String) extends LambdaTerm
final case class Abstraction(variable: Variable, term: LambdaTerm) extends LambdaTerm
final case class Application(term: LambdaTerm, toApplyTo: LambdaTerm) extends LambdaTerm

object LambdaTerm {
  def prettyPrint(lambdaTerm: LambdaTerm): String = lambdaTerm match {
    case Variable(name: String) => name
    case Abstraction(variable, term) => s"λ${variable.name}.${prettyPrint(term)}"
    case Application(term: LambdaTerm, toApplyTo: LambdaTerm) => s"(${prettyPrint(term)} ${prettyPrint(toApplyTo)})"
  }

  def fromNormalLambdaToDeBruijn(lambdaTerm: LambdaTerm): DeBruijnLambdaTerm =
    fromNormalLambdaToDeBruijnRec(lambdaTerm, Map.empty, Zero)

  private def fromNormalLambdaToDeBruijnRec(lambdaTerm: LambdaTerm,
                                            variableBindings: Map[Variable, NaturalNum],
                                            depth: NaturalNum): DeBruijnLambdaTerm = {
    lambdaTerm match {
      case variable @ Variable(_) =>
        variableBindings.get(variable) match {
          case Some(associatedIdx) =>
            DeBruijnIndex(associatedIdx)
          case None =>
            DeBruijnIndex(Succ(depth))
        }
      case Abstraction(variable, appliedTerm) =>
        val incrementedBindings = variableBindings.mapValues(NaturalNum.increment)
        DeBruijnAbstraction(fromNormalLambdaToDeBruijnRec(appliedTerm, incrementedBindings + (variable -> Zero), Succ(depth)))
      case Application(applier, applied) =>
        DeBruijnApplication(
          fromNormalLambdaToDeBruijnRec(applier, variableBindings, depth),
          fromNormalLambdaToDeBruijnRec(applied, variableBindings, depth)
        )
    }
  }

  val identityLambda: LambdaTerm = Abstraction(Variable("x"), Variable("x"))

  def fromNaturalNum(num: NaturalNum): LambdaTerm =
    Abstraction(Variable("f"), Abstraction(Variable("x"), fromNaturalNumCompositionTerm(num)))

  private def fromNaturalNumCompositionTerm(num: NaturalNum): LambdaTerm = num match {
    case Zero =>
      Variable("x")
    case Succ(prev) =>
      Application(Variable("f"), fromNaturalNumCompositionTerm(prev))
  }

  def retrieveFreeVariables(lambdaTerm: LambdaTerm): Set[Variable] =
    retrieveFreeVariablesRec(lambdaTerm, Set.empty)

  private def retrieveFreeVariablesRec(lambdaTerm: LambdaTerm, currentSet: Set[Variable]): Set[Variable] = lambdaTerm match {
    case variable @ Variable(_) =>
      currentSet + variable
    case Abstraction(variable, term) =>
      retrieveFreeVariablesRec(term, currentSet - variable)
    case Application(applier, applied) =>
      retrieveFreeVariablesRec(applier, currentSet) ++ retrieveFreeVariablesRec(applied, currentSet)
  }
}
