package com.changlinli.computability

import org.scalatest.FlatSpec

class TestLambdaTerm extends FlatSpec {
  "The De Bruijn representation of the identity function" should "agree with the Lambda representation of the identity" in {
    assert(DeBruijnLambdaTerm.fromNormalLambda(LambdaTerm.identityLambda) === DeBruijnLambdaTerm.identity)
  }
  "Beta reduction of the successor lambda applied to a Church encoded numeral" should "agree with the successor function applied on natural numbers" in {
    val lambda5 = LambdaTerm.fromNaturalNum(NaturalNum.fromInt(5).get)
    val applySuccessorTo5 = Application(LambdaTerm.successorLambda, lambda5)
    val lambda6 = LambdaTerm.fromNaturalNum(NaturalNum.fromInt(6).get)
    val deBruijnResult = DeBruijnLambdaTerm.fromNormalLambda(LambdaTerm.betaReduce(applySuccessorTo5))
    val deBruijnLambda6 = DeBruijnLambdaTerm.fromNormalLambda(lambda6)
    withClue(s"Our result was ${DeBruijnLambdaTerm.prettyPrint(deBruijnResult)}\nOur expectation was ${DeBruijnLambdaTerm.prettyPrint(deBruijnLambda6)}\n") {
      assert(deBruijnResult === deBruijnLambda6)
    }
  }
  "Beta reduction of the addition lambda applied to a Church encoded numeral" should "agree with addition on the natural numbers" in {
    val naturalNum5 = NaturalNum.fromInt(5).get
    val lambda5 = LambdaTerm.fromNaturalNum(naturalNum5)
    val naturalNum3 = NaturalNum.fromInt(3).get
    val lambda3 = LambdaTerm.fromNaturalNum(naturalNum3)
    val naturalNum8 = NaturalNum.fromInt(8).get
    val lambda8 = LambdaTerm.fromNaturalNum(naturalNum8)
    val additionResult = LambdaTerm.betaReduce(Application(Application(LambdaTerm.additionLambda, lambda5), lambda3))
    assert(LambdaTerm.alphaEquivalent(additionResult, lambda8))
  }
}
