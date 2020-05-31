package com.maria.epam.lab

import scala.annotation.tailrec

object Main {

  @tailrec
  def foldl[A, B](f: (B, A) => B, a: B, c: List[A]): B = c match {
    case Nil => a
    case r :: rest => foldl(f, f(a, r), rest)
  }

  def sum[A](identity: A, compose: (A, A) => A)(in: List[A]): A = in match {
    case x :: xs => compose(x, sum(identity, compose)(xs))
    case Nil => identity
  }

  def sum_tail[A](identity: A, compose: (A, A) => A)(in: List[A]): A =
    foldl[A, A]((x, y) => compose(x, y), identity, in)

  def main(args: Array[String]): Unit = {
    val int_sum: List[Int] => Int = sum[Int](0, (x, y) => x + y)
    println(int_sum(List(1, 2, 3, 4, 5, 6, 20)))
    val int_sum_tail: List[Int] => Int = sum_tail[Int](0, (x, y) => x + y)
    println(int_sum_tail(List(2, 3, 4, 5)))
  }

}
