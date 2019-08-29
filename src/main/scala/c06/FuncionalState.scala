package c06

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Exercise {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, r) if i >= 0 => (i, r)
      case (_, r)           => nonNegativeInt(r)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (i.toDouble, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = r.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    1.to(count).foldLeft((Nil: List[Int], rng)) {
      case ((list, r), _) =>
        val (i, r2) = r.nextInt
        (i :: list, r2)
    }
  }

  //------------------------------------
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def mapAlter[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f.andThen(unit))

  def tuple[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = { rng =>
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    ((a, b), r2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, ra1) = ra(rng)
      val (b, ra2) = rb(ra1)
      val c = f(a, b)
      (c, ra2)
  }

  def map2Alter[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    f(a)(rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s2) = run(s)
    (f(a), s2)
  }

  def mapAlter[B](f: A => B): State[S, B] = flatMap(f.andThen(State.unit))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }

  def map2Alter[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }

  def modify(f: S => S): State[S, A] = State { s=>
    val (a, s2) = run(s)
    (a, f(s2))
  }

  def modifyAlter(f: S => S): State[S, Unit] = for {
    s <- State.get
    _ <- State.set(f(s))
  } yield ()
}
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    val init: State[S, List[A]] = unit(Nil)
    fs.foldRight(init)((f, acc) => f.map2Alter(acc)(_ :: _))
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object CandyDispense {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)
}
