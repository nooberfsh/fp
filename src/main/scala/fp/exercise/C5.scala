package fp.exercise

object C5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    // c5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    // c5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    // c5.2
    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    // c5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty
    }

    //////////////////////////////////////////////////////////////////////////////

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def _foldRight[B](z: => B)(f: (=> A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t()._foldRight(z)(f))
        case _ => z
      }

    def tail: Stream[A] = this match {
      case Cons(_, t) => t()
      case _ => Stream.empty
    }

    // c5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // c5.5
    def _takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) {
      case (a, b) if p(a) => Stream.cons(a, b)
      case _ => Empty
    }

    // c5.6
    def _headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

    // c5.7
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

    def ___map[B](f: A => B): Stream[B] =
      _foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

    def __map[B](f: A => B): Stream[B] = this match {
      case Cons(h, t) => Stream.cons(f(h()), t().__map(f))
      case Empty => Empty
    }


    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) {
        case (a, b) if p(a) => Stream.cons(a, b)
        case (_, b) => b
      }

    def _filter(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t()._filter(p))
      case _ => Empty
    }

    def __filter(p: A => Boolean): Stream[A] =
      Stream._unfold(this) {
        case Cons(h, t) if p(h()) => Some((Some(h()), t()))
        case Cons(_, t) => Some((None, t()))
        case _ => None
      }

    def appendOne[B >: A](b: => B): Stream[B] =
      foldRight(Stream(b))((a, b) => Stream.cons(a, b))

    def append[B >: A](b: => Stream[B]): Stream[B] =
      foldRight(b)((a, b) => Stream.cons(a, b))

    def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((a, b) => f(a).append(b))

    // c5.13
    def _map[B](f: A => B): Stream[B] =
      Stream.unfold(this) {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      }

    def _take(n: Int): Stream[A] =
      Stream.unfold((n, this)) {
        case (k, Cons(h, t)) if k > 0 => Some(h(), (n - 1, t()))
        case _ => None
      }

    def __takeWhile(p: A => Boolean): Stream[A] =
      Stream.unfold(this) {
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }

    def zipWith[B](s: Stream[B]): Stream[(A, B)] =
      Stream.unfold((this, s)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
        case _ => None
      }

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
      Stream.unfold((this, s)) {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
        case _ => None
      }

    def exists(p: A => Boolean): Boolean =
      this.foldRight(false)((a, b) => p(a) || b)

    def hasSubsequence[B >: A](sub: Stream[B]): Boolean = {
      Stream
        .unfold(this) {
          case s@Cons(_, t) => Some(s.zipAll(sub), t())
          case _ => None
        }
        .appendOne(Stream.empty.zipAll(sub))
        .exists(_.forAll { case (a, b) => a == b })
    }

    // c5.14
    def startsWith[B >: A](s: Stream[B]): Boolean = (this, s) match {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        (h1() == h2()) && t1().startsWith(t2())
      case (Empty, Cons(_, _)) => false
      case (Cons(_, _), Empty) => true
      case (Empty, Empty) => true
    }

    def _startsWith[B >: A](s: Stream[B]): Boolean =
      zipAll(s).takeWhile(_._2.isDefined).forAll { case (a, b) => a == b }

    // c5.15
    def tails: Stream[Stream[A]] = {
      Stream.unfold(this) {
        case s@Cons(_, t) => Some(s, t())
        case _ => None
      } appendOne Stream.empty
    }

    // c5.16
    def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
      val f2 = (a: A, b: B, s: Stream[B]) => {
        val c = f(a, b)
        (c, Stream.cons(c, s))
      }

      def go(z: B, s: Stream[A])(
        f: (A, B, Stream[B]) => (B, Stream[B])): (B, Stream[B]) = s match {
        case Cons(h, t) =>
          val (b, sb) = go(z, t())(f)
          f(h(), b, sb)
        case Empty => (z, Stream.apply(z))
      }

      go(z, this)(f2)._2
    }

    def _scanRight[B](z: B)(f: (A, B) => B): Stream[B] =
      foldRight((z, Stream(z))) {
        case (a, (b, s)) =>
          val c = f(a, b)
          (c, Stream.cons(c, s))
      }._2

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

    // c5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def zip[A, B, C](l: Stream[A], r: Stream[B])(f: (A, B) => C): Stream[C] =
      (l, r) match {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          cons(f(h1(), h2()), zip(t1(), t2())(f))
        case _ => empty[C]
      }

    // c5.10
    def fibs: Stream[Int] = cons(0, cons(1, zip(fibs.tail, fibs)(_ + _)))

    //////////////////////////////////////////////////////////////////////////////

    // c5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None => empty
      }

    def _unfold[A, S](z: S)(f: S => Option[(Option[A], S)]): Stream[A] =
      f(z) match {
        case Some((a, s)) => if (a.isDefined) cons(a.get, _unfold(s)(f)) else _unfold(s)(f)
        case _ => empty
      }

    //    def __unfold[A, S](z: S)(f: S => ?(?A, S) : Stream[A]

    // c5.12
    def _fibs: Stream[Int] = unfold((0, 1)) {
      case (a, b) =>
        Some((a, (b, a + b)))
    }

    def _from(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

    def _constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

    def _ones: Stream[Int] = unfold(())(_ => Some(1, ()))

  }

}
