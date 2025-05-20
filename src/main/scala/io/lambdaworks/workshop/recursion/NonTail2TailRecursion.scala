package io.lambdaworks.workshop.recursion

import java.util.concurrent.atomic.DoubleAccumulator
import scala.annotation.tailrec

/**
  * Rewrite below non tail-recursive functions to tail-recursive one.
  * Add @tailrec annotation to prove it.
  */
object NonTail2TailRecursion {

  def factorial(n: Int): Int =
    if (n <= 0) 1 else n * factorial(n - 1)

  def factorialTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accumulator: Int = 0): Int =
      if (n <= 1) accumulator else loop(n - 1, accumulator * n)

    loop(n)
  }


  def cubesOfEvens(numbers: List[Double]): List[Double] =
    numbers match {
      case x :: xs if x % 2 == 0 => Math.pow(x, 3) +: cubesOfEvens(xs)
      case _ :: xs => cubesOfEvens(xs)
      case Nil     => List.empty
    }

  def cubesOfEvensTailRec(numbers: List[Double]) : List[Double] = {
    @tailrec
    def loop(numbers: List[Double], accumulator: List[Double] ) : List[Double] =
      numbers match {
        case x :: xs if x % 2 == 0 => loop(xs, Math.pow(x, 3)::accumulator)
        case _ :: xs => loop(xs, accumulator)
        case Nil     => accumulator
      }

    loop(numbers, Nil)
  }

}
