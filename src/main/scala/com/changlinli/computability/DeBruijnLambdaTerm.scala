package com.changlinli.computability

sealed trait DeBruijnLambdaTerm
final case class DeBruijnIndex(idx: NaturalNum) extends DeBruijnLambdaTerm
final case class DeBruijnAbstraction(term: DeBruijnLambdaTerm) extends DeBruijnLambdaTerm
final case class DeBruijnApplication(term: DeBruijnLambdaTerm, toApplyTo: DeBruijnLambdaTerm) extends DeBruijnLambdaTerm

object DeBruijnLambdaTerm {
  def identity: DeBruijnLambdaTerm = DeBruijnAbstraction(DeBruijnIndex(Zero))

  def prettyPrint(deBruijnLambdaTerm: DeBruijnLambdaTerm): String = deBruijnLambdaTerm match {
    case DeBruijnIndex(idx) => NaturalNum.toInt(idx).toString
    case DeBruijnAbstraction(term) => s"λ ${prettyPrint(term)}"
    case DeBruijnApplication(term, toApplyTo) => s"(${prettyPrint(term)} ${prettyPrint(toApplyTo)})"
  }

  def fromDeBruijnToNormalLambda(deBruijnLambdaTerm: DeBruijnLambdaTerm,
                                 freshVariables: InfiniteStream[Variable] = InfiniteStream.neverEndingVars): LambdaTerm =
    fromDeBruijnToNormalLambdaRec(deBruijnLambdaTerm, Map.empty, freshVariables)._1

  def fromDeBruijnToNormalLambdaRec(deBruijnLambdaTerm: DeBruijnLambdaTerm,
                                    variableIndices: Map[DeBruijnIndex, Variable],
                                    freshVariables: InfiniteStream[Variable]): (LambdaTerm, InfiniteStream[Variable]) = deBruijnLambdaTerm match {
    case idx @ DeBruijnIndex(_) =>
      (variableIndices.getOrElse(idx, freshVariables.head), freshVariables.tail)
    case DeBruijnAbstraction(term) =>
      val newVariable = freshVariables.head
      val incrementedIndices =
        variableIndices.map{case (DeBruijnIndex(index), variable) => DeBruijnIndex(NaturalNum.increment(index)) -> variable}
      val (lambda, remainingFreshVariables) =
        fromDeBruijnToNormalLambdaRec(term, incrementedIndices + (DeBruijnIndex(Zero) -> newVariable), freshVariables.tail)
      (Abstraction(newVariable, lambda), remainingFreshVariables)
    case DeBruijnApplication(term, toApplyTo) =>
      val (applier, newVariables) = fromDeBruijnToNormalLambdaRec(term, variableIndices, freshVariables)
      val (applied, newerVariables) = fromDeBruijnToNormalLambdaRec(toApplyTo, variableIndices, newVariables)
      val lambdaResult = Application(applier, applied)
      (lambdaResult, newerVariables)
  }
}
