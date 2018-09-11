package com.changlinli.computability

import cats.implicits._
import org.scalacheck.Arbitrary
sealed trait DeBruijnLambdaTerm
final case class DeBruijnIndex(idx: NaturalNum) extends DeBruijnLambdaTerm
final case class DeBruijnAbstraction(term: DeBruijnLambdaTerm) extends DeBruijnLambdaTerm
final case class DeBruijnApplication(term: DeBruijnLambdaTerm, toApplyTo: DeBruijnLambdaTerm) extends DeBruijnLambdaTerm

object DeBruijnLambdaTerm {
  def fromNaturalNum(x: NaturalNum): DeBruijnLambdaTerm =
    fromNormalLambda(LambdaTerm.fromNaturalNum(x))

  def fromNormalLambda(lambdaTerm: LambdaTerm): DeBruijnLambdaTerm =
    fromNormalLambdaRec(lambdaTerm, Map.empty, Zero)

  private def fromNormalLambdaRec(lambdaTerm: LambdaTerm,
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
        DeBruijnAbstraction(fromNormalLambdaRec(appliedTerm, incrementedBindings + (variable -> Zero), Succ(depth)))
      case Application(applier, applied) =>
        DeBruijnApplication(
          fromNormalLambdaRec(applier, variableBindings, depth),
          fromNormalLambdaRec(applied, variableBindings, depth)
        )
    }
  }

  def identity: DeBruijnLambdaTerm = DeBruijnAbstraction(DeBruijnIndex(Zero))

  private def betaReduceThroughOnce(term: DeBruijnLambdaTerm): DeBruijnLambdaTerm = {
    term match {
      case DeBruijnApplication(abstraction @ DeBruijnAbstraction(underAbstraction), toApplyTo) =>
        substitutionRec(underAbstraction, toApplyTo, Zero)
      case DeBruijnApplication(notAbstraction, toApplyTo) =>
        DeBruijnApplication(betaReduceThroughOnce(notAbstraction), betaReduceThroughOnce(toApplyTo))
      case DeBruijnAbstraction(underAbstraction) =>
        DeBruijnAbstraction(betaReduceThroughOnce(underAbstraction))
      case index @ DeBruijnIndex(_) =>
        index
    }
  }

  private def incrementFreeByAmount(term: DeBruijnLambdaTerm, amount: NaturalNum): DeBruijnLambdaTerm =
    incrementFreeByAmountRec(term, amount, Zero)

  private def incrementFreeByAmountRec(term: DeBruijnLambdaTerm, amount: NaturalNum, depth: NaturalNum): DeBruijnLambdaTerm = {
    term match {
      // Bound variable
      case DeBruijnIndex(idx) if NaturalNum.increment(idx) <= depth =>
        DeBruijnIndex(idx)
      // Free variable
      case DeBruijnIndex(idx) if NaturalNum.increment(idx) > depth =>
        DeBruijnIndex(NaturalNum.plus(idx, amount))
      case DeBruijnAbstraction(innerTerm) =>
        DeBruijnAbstraction(incrementFreeByAmountRec(innerTerm, amount, Succ(depth)))
      case DeBruijnApplication(innerTerm, toApplyTo) =>
        DeBruijnApplication(
          incrementFreeByAmountRec(innerTerm, amount, depth),
          incrementFreeByAmountRec(toApplyTo, amount, depth)
        )
    }
  }

  // λ λ λ (2 (1 0))      λ λ (1 0)
  // L L ((L L (1 0)) (1 0))
  // L L ((L ((L L (1 0)) 0)) (1 0))
  // L L ((L ((L L (1 0)) 0)) (1 0))
  private def substitutionRec(parentTerm: DeBruijnLambdaTerm,
                              substitute: DeBruijnLambdaTerm,
                              depth: NaturalNum): DeBruijnLambdaTerm = {
    parentTerm match {
      case DeBruijnIndex(idx) if idx == depth =>
        incrementFreeByAmount(substitute, depth)
      // This means the variable is free
      case DeBruijnIndex(idx) if idx > depth =>
        // Note we should never hit floor because idx is strictly larger than depth which at minimum is zero
        DeBruijnIndex(NaturalNum.decrementOrFloor(idx))
      // This means the variable is already bound, just by an inner lambda
      case DeBruijnIndex(idx) =>
        DeBruijnIndex(idx)
      case DeBruijnAbstraction(term) =>
        DeBruijnAbstraction(substitutionRec(term, substitute, Succ(depth)))
      case DeBruijnApplication(term, toApplyTo) =>
        DeBruijnApplication(
          substitutionRec(term, substitute, depth),
          substitutionRec(toApplyTo, substitute, depth)
        )
    }
  }

  def betaReduce(term: DeBruijnLambdaTerm): DeBruijnLambdaTerm = {
    val result = betaReduceThroughOnce(term)
    if (result == term) {
      result
    } else {
      betaReduce(result)
    }
  }

  def prettyPrint(deBruijnLambdaTerm: DeBruijnLambdaTerm): String = deBruijnLambdaTerm match {
    case DeBruijnIndex(idx) => NaturalNum.toInt(idx).toString
    case DeBruijnAbstraction(term) => s"λ ${prettyPrint(term)}"
    case DeBruijnApplication(term, toApplyTo) => s"(${prettyPrint(term)} ${prettyPrint(toApplyTo)})"
  }

}
