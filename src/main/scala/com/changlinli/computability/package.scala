package com.changlinli

import atto.Parser

package object computability {
  implicit class MoreAttoOperators[A](atto: Parser[A]) {
    def covary[B >: A]: Parser[B] = atto.map(identity[B])
  }

  implicit class StdOps[A](x: A) {
    def |>[B](f: A => B): B = f(x)
  }

}
