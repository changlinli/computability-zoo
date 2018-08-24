package com.changlinli.computability

import cats.Eval

final case class InfiniteStream[A](head: A, rest: Eval[InfiniteStream[A]]) {
  def map[B](f: A => B): InfiniteStream[B] = InfiniteStream(f(head), rest.map(_.map(f)))

  def tail: InfiniteStream[A] = rest.value

  def take(n: Int): List[A] =
    if (n <= 0) List.empty else head :: tail.take(n - 1)
}

object InfiniteStream {
  val allNaturalNums: InfiniteStream[NaturalNum] = iterate(NaturalNum.zero)(NaturalNum.increment)

  val neverEndingLetters: InfiniteStream[String] =
    allNaturalNums.map(NaturalNum.toInt).map(idx => s"x$idx")

  val neverEndingVars: InfiniteStream[Variable] = neverEndingLetters.map(Variable.apply)

  def iterate[A](start: A)(next: A => A): InfiniteStream[A] =
    unfold(start)(x => (next(x), x))

  def unfold[A, B](start: A)(f: A => (A, B)): InfiniteStream[B] = {
    val (state, elem) = f(start)
    InfiniteStream(elem, Eval.later(unfold(state)(f)))
  }
}
