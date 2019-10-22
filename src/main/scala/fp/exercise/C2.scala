package fp.exercise

import scala.annotation.tailrec

object C2 {
  // c2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, l: Int, r: Int): (Int, Int) = {
      if (n == 0) (l, r)
      else go(n - 1, r, l + r)
    }
    go(n, 0, 1)._1
  }

  // c2.2
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as match {
      case Array() | Array(_) => true
      case _ =>
        val head = as(0)
        // TODO: eliminate alloc
        val rest = as.slice(1, as.length)
        if (ordered(head, rest(0))) isSorted(rest, ordered)
        else false
    }

  // c2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { a =>
    f(a, _)
  }

  // c2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) =>
    f(a)(b)
  }

  // c2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = { a =>
    f(g(a))
  }

  def andThen[A, B, C](f: A => B, g: B => C): A => C = { a =>
    g(f(a))
  }
}
