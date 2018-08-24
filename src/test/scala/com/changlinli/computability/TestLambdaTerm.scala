package com.changlinli.computability

import org.scalatest.FlatSpec

class TestLambdaTerm extends FlatSpec {
  "The De Bruijn representation of the identity function" should "agree with the Lambda representation of the identity" in {
    assert(LambdaTerm.fromNormalLambdaToDeBruijn(LambdaTerm.identityLambda) === DeBruijnLambdaTerm.identity)
  }
}
