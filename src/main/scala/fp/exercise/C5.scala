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

    def foldRight[B](b: B)(f: (A, =>B) => B): B = this match {
      case Empty => b
      case Cons(h, t) => f(h(), t().foldRight(b)(f))
    }


    // c5.2
    def take(n: Int): List[A] = {
      def take(n: Int, stream: Stream[A], acc: List[A]): List[A] = n match {
        case _ if n <=0 => acc
        case _ => stream match {
          case Empty => acc
          case Cons(h, t) => h() :: take(n - 1, t(), acc)
        }
      }
      take(n, this, Nil)
    }

    def _take(n: Int): List[A] = this.foldRight(Nil: List[A]) { (a, b) =>
      if (n <= 0) Nil else a :: b
    }

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
