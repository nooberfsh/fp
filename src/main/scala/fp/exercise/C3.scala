package fp.exercise

import fp.c03._

import scala.annotation.tailrec

object C3 {
  // c3.2
  def tail[A](as: List[A]): Option[List[A]] = as match {
    case Nil         => None
    case Cons(_, xs) => Some(xs)
  }

  // c3.3
  def setHead[A](as: List[A], h: A): Option[List[A]] = as match {
    case Nil         => None
    case Cons(_, xs) => Some(Cons(h, xs))
  }

  // c3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => if (n <= 0) l else drop(xs, n - 1)
  }

  // c3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  // c3.6
  // TODO: not tailrec sad!
  def init[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  //////////////////////////////////////////////////////////////////////////////

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f)) // not tailrec
  }

  def product(l: List[Double]): Double =
    foldRight(l, 0.0)(_ * _)

  // c3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, ret) => ret + 1)

  // c3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // c3.11
  def sum2(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product2(l: List[Double]): Double = foldLeft(l, 0.0)(_ * _)

  // c3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // c3.13
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val reversed = reverse(as)
    foldLeft(reversed, z)((b, a) => f(a, b))
  }

  // c3.14
  def appendOne[A](as: List[A], x: A): List[A] =
    foldRight2(as, List(x))(Cons.apply)

  // c3.15
  def concat[A](l: List[A], x: List[A]): List[A] = foldRight(l, x)(Cons.apply)
  def concatMulti[A](xss: List[List[A]]): List[A] =
    foldRight(xss, Nil: List[A])(concat)

  //////////////////////////////////////////////////////////////////////////////

  // c3.16
  def mapIntInc(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // c3.17
  def mapDoubleToStr(l: List[Int]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  // c3.18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  // c3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A]) {
      case (a, b) if f(a) => Cons(a, b)
      case (_, b)         => b
    }

  // c3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((a, b) => concat(f(a), b))

  // c3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) {
    case a if f(a) => List(a)
    case _         => Nil
  }

  // c3.22
  def zipInt(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Cons(lx, lxs), Cons(rx, rxs)) => Cons(lx + rx, zipInt(lxs, rxs))
    case (a, Nil)                       => a
    case (Nil, b)                       => b
  }

  // c3.23
  def zipWith[A, B](l: List[A], r: List[B]): List[(A, B)] = (l, r) match {
    case (Cons(lx, lxs), Cons(rx, rxs)) => Cons((lx, rx), zipWith(lxs, rxs))
    case (_, Nil)                       => Nil
    case (Nil, _)                       => Nil
  }

  // c3.24
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def isPrefix(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil)                       => true
      case (Cons(lx, lxs), Cons(rx, rxs)) => lx == rx && isPrefix(lxs, rxs)
      case _                              => false
    }

    sup match {
      case Nil                     => sub == Nil
      case _ if isPrefix(sup, sub) => true
      case Cons(_, xs)             => hasSubsequence(xs, sub)
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  sealed trait Tree[+A] {
    // c3.25
    def size: Int = fold(_ => 1)((l, r) => l + r + 1)

    // FIXME: covariant type A occurs in invariant position in type Ordering[A] of value ordering
    // c3.26
//    def maximum(implicit ordering: Ordering[A]): A = this match {
//      case Leaf(d)      => d
//      case Branch(l, r) => ordering.max(l.maximum, r.maximum)
//    }

    // c3.27
    def depth: Int = fold(_ => 1)((l, r) => (l max r) + 1)

    // c3.28
    def map[B](f: A => B): Tree[B] =
      fold(a => Leaf(f(a)): Tree[B])((t1, t2) => Branch(t1, t2))

    // c3.29
    def fold[B](init: A => B)(f: (B, B) => B): B = this match {
      case Leaf(a)      => init(a)
      case Branch(l, r) => f(l.fold(init)(f), r.fold(init)(f))
    }
  }
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // branch has a value
  sealed trait Tree2[+A] {
    def fold[B](init: A => B)(f: (A, B, B) => B): B = this match {
      case Leaf2(a)           => init(a)
      case Branch2(a, t1, t2) => f(a, t1.fold(init)(f), t2.fold(init)(f))
    }

    def size: Int = fold(_ => 1)((_, l, r) => l + r + 1)

    def depth: Int = fold(_ => 1)((_, l, r) => (l max r) + 1)

    def map[B](f: A => B): Tree2[B] =
      fold(a => Leaf2(f(a)): Tree2[B])((a, b1, b2) => Branch2(f(a), b1, b2))
  }
  case class Branch2[A](value: A, left: Tree2[A], right: Tree2[A])
      extends Tree2[A]
  case class Leaf2[A](value: A) extends Tree2[A]

  // leaf has no value
  sealed trait Tree3[+A] {
    def fold[B](b: B)(f: (A, B, B) => B): B = this match {
      case Leaf3              => b
      case Branch3(a, t1, t2) => f(a, t1.fold(b)(f), t2.fold(b)(f))
    }

    def size: Int = fold(1)((_, l, r) => l + r + 1)

    def depth: Int = fold(1)((_, l, r) => (l max r) + 1)

    def map[B](f: A => B): Tree3[B] =
      fold(Leaf3: Tree3[B])((a, b1, b2) => Branch3(f(a), b1, b2))
  }
  case class Branch3[A](value: A, left: Tree3[A], right: Tree3[A])
      extends Tree3[A]
  case object Leaf3 extends Tree3[Nothing]
}
