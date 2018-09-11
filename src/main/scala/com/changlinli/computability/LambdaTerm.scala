package com.changlinli.computability

sealed trait LambdaTerm
final case class Variable(name: String) extends LambdaTerm
final case class Abstraction(variable: Variable, term: LambdaTerm) extends LambdaTerm
final case class Application(term: LambdaTerm, toApplyTo: LambdaTerm) extends LambdaTerm

object LambdaTerm {
  def alphaEquivalent(term0: LambdaTerm, term1: LambdaTerm): Boolean =
    DeBruijnLambdaTerm.fromNormalLambda(term0) == DeBruijnLambdaTerm.fromNormalLambda(term1)

  def betaReduce(application: Application): LambdaTerm =
    fromDeBruijn(DeBruijnLambdaTerm.betaReduce(DeBruijnLambdaTerm.fromNormalLambda(application)))

  def prettyPrint(lambdaTerm: LambdaTerm): String = lambdaTerm match {
    case Variable(name: String) => name
    case Abstraction(variable, term) => s"λ${variable.name}.${prettyPrint(term)}"
    case Application(term: LambdaTerm, toApplyTo: LambdaTerm) => s"(${prettyPrint(term)} ${prettyPrint(toApplyTo)})"
  }

  def fromDeBruijn(deBruijnLambdaTerm: DeBruijnLambdaTerm,
                   freshVariables: InfiniteStream[Variable] = InfiniteStream.neverEndingVars): LambdaTerm =
    fromDeBruijnRec(deBruijnLambdaTerm, Map.empty, freshVariables)._1

  def fromDeBruijnRec(deBruijnLambdaTerm: DeBruijnLambdaTerm,
                      variableIndices: Map[DeBruijnIndex, Variable],
                      freshVariables: InfiniteStream[Variable]): (LambdaTerm, InfiniteStream[Variable]) = deBruijnLambdaTerm match {
    case idx @ DeBruijnIndex(_) =>
      (variableIndices.getOrElse(idx, freshVariables.head), freshVariables.tail)
    case DeBruijnAbstraction(term) =>
      val newVariable = freshVariables.head
      val incrementedIndices =
        variableIndices.map{case (DeBruijnIndex(index), variable) => DeBruijnIndex(NaturalNum.increment(index)) -> variable}
      val (lambda, remainingFreshVariables) =
        fromDeBruijnRec(term, incrementedIndices + (DeBruijnIndex(Zero) -> newVariable), freshVariables.tail)
      (Abstraction(newVariable, lambda), remainingFreshVariables)
    case DeBruijnApplication(term, toApplyTo) =>
      val (applier, newVariables) = fromDeBruijnRec(term, variableIndices, freshVariables)
      val (applied, newerVariables) = fromDeBruijnRec(toApplyTo, variableIndices, newVariables)
      val lambdaResult = Application(applier, applied)
      (lambdaResult, newerVariables)
  }

  val identityLambda: LambdaTerm = Abstraction(Variable("x"), Variable("x"))

  val successorLambda: LambdaTerm =
    Abstraction(Variable("n"), Abstraction(Variable("f"), Abstraction(Variable("x"), Application(Variable("f"), Application(Application(Variable("n"), Variable("f")), Variable("x"))))))

  val additionLambda: LambdaTerm =
    Abstraction(Variable("m"), Abstraction(Variable("n"), Abstraction(Variable("f"), Abstraction(Variable("x"), Application(Application(Variable("m"), Variable("f")), Application(Application(Variable("n"), Variable("f")), Variable("x")))))))

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
