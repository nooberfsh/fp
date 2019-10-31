package fp.exercise

object C5 {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, _) => Some(h())
    }

    // c5.1
    def toList: List[A] = this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

    // c5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _                   => Empty
    }

    // c5.2
    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _                   => this
    }

    // c5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _                    => Empty
    }

    //////////////////////////////////////////////////////////////////////////////

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _          => z
      }

    def tail: Stream[A] = this match {
      case Cons(h, t) => t()
      case _          => Stream.empty
    }

    // c5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // c5.5
    def _takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) {
      case (a, b) if p(a) => Stream.cons(a, b)
      case _              => Empty
    }

    // c5.6
    def _headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

    // c5.7
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) {
        case (a, b) if p(a) => Stream.cons(a, b)
        case (_, b)         => b
      }

    def appendOne[B >: A](b: => B): Stream[B] =
      foldRight(Stream.cons(b, Stream.empty))((a, b) => Stream.cons(a, b))

    def append[B >: A](b: => Stream[B]): Stream[B] =
      foldRight(b)((a, b) => Stream.cons(a, b))

    def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      // Memoizing streams and avoiding recomputation
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    //////////////////////////////////////////////////////////////////////////////

    // c5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    // c5.8
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def zip[A, B, C](l: Stream[A], r: Stream[B])(f: (A, B) => C): Stream[C] =
      (l, r) match {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          cons(f(h1(), h2()), zip(t1(), t2())(f))
        case _ => empty[C]
      }

    // c5.9
    def fibs: Stream[Int] = cons(0, cons(1, zip(fibs.tail, fibs)(_ + _)))
  }
}
