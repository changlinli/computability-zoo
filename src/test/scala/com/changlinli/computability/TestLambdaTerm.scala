package com.changlinli.computability

import org.scalatest.FlatSpec

class TestLambdaTerm extends FlatSpec {
  "The De Bruijn representation of the identity function" should "agree with the Lambda representation of the identity" in {
    assert(LambdaTerm.fromNormalLambdaToDeBruijn(LambdaTerm.identityLambda) === DeBruijnLambdaTerm.identity)
  }
  "Beta reduction of the successor lambda applied to a Church encoded numeral" should "agree with the successor function applied on natural numbers" in {
    val lambda5 = LambdaTerm.fromNaturalNum(NaturalNum.fromInt(5).get)
    val applySuccessorTo5 = Application(LambdaTerm.successorLambda, lambda5)
    val lambda6 = LambdaTerm.fromNaturalNum(NaturalNum.fromInt(6).get)
    val deBruijnResult = LambdaTerm.fromNormalLambdaToDeBruijn(LambdaTerm.betaReduce(applySuccessorTo5))
    val deBruijnLambda6 = LambdaTerm.fromNormalLambdaToDeBruijn(lambda6)
    withClue(s"Our result was ${DeBruijnLambdaTerm.prettyPrint(deBruijnResult)}\nOur expectation was ${DeBruijnLambdaTerm.prettyPrint(deBruijnLambda6)}\n") {
      assert(deBruijnResult === deBruijnLambda6)
    }
  }
}
