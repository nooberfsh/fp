package fp.exercise

object C5 {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // c5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    // c5.2
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n>0 =>  Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    // c5.2
    def drop(n: Int): Stream[A] = this match {
      case Cons(h, t) if n>0 =>  t().drop(n - 1)
      case _ => this
    }

    // c5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty
    }

    //////////////////////////////////////////////////////////////////////////////


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
  }
}
