package fp.exercise

import scala.annotation.tailrec

object C6 {

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

  // c6.1
  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) if i >= 0 => (i, r)
      case (_, r)           => nonNegativeInt(r)
    }

  // c6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), r)
  }

  // c6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // c6.3
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldLeft((Nil: List[Int], rng)) {
      case ((l, r), _) =>
        val (i, r2) = r.nextInt
        (i :: l, r2)
    }

  //////////////////////////////////////////////////////////////////////////////

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, r1) = r(rng)
      (f(a), r1)
    }

  // c6.5
  val doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  // c6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
  }

  // c6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs.foldLeft((Nil: List[A], rng)) {
      case ((l, r), rand) =>
        val (i, r2) = rand(r)
        (i :: l, r2)
    }
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

  def ints2(count: Int): Rand[List[Int]] = {
    val fs = List.fill(count)(int)
    sequence2(fs)
  }

  //////////////////////////////////////////////////////////////////////////////

  // c6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, ra) = f(rng)
      g(a)(ra)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      if (i < n) unit(i)
      else nonNegativeLessThan(n)
    }

  // c6.9
  def mapExt[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2Ext[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapExt(rb)(b => f(a, b)))

  //////////////////////////////////////////////////////////////////////////////

  // c6.10
  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = State { s =>
      val (a, newS) = run(s)
      (f(a), newS)
    }

    def _map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- this
        b <- sb
      } yield f(a, b)

    def _map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, newS) = run(s)
      f(a).run(newS)
    }

    def apply(s: S): (A, S) = run(s)

    def join[B, C](sb: State[S, B]): State[S, (A, B)] = map2(sb)((_, _))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] =
      xs.foldLeft(unit[S, List[A]](Nil))((b, a) => b.map2(a)((l, h) => h :: l))
        .map(_.reverse)

    // note: state goes from left to right: s1, s2, s3
    // map2(s1, map2(s2, map2(s3, ...)))
    def _sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] =
      xs.foldRight(unit[S, List[A]](Nil))((a, b) => a.map2(b)((i, l) => i :: l))

    // note: state goes from right to left: s3, s2, s1
    // map2(map2(map2(s3, ...), s2), s1)
    def sequenceReverse[S, A](xs: List[State[S, A]]): State[S, List[A]] =
      xs.foldRight(unit[S, List[A]](Nil))((a, b) => b.map2(a)((l, i) => i :: l))

    def get[S]: State[S, S] = State { s =>
      (s, s)
    }

    def set[S](s: S): State[S, Unit] = State { _ =>
      ((), s)
    }

    def modify[S](f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(f(s))
      } yield ()

    def _modify[S](f: S => S): State[S, Unit] = State { s =>
      val s2 = f(s)
      ((), s2)
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  // c6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Machine {
    import State.{sequence => xsequence, _}

    def update: Input => Machine => Machine =
      (i: Input) =>
        (s: Machine) =>
          (i, s) match {
            case (_, Machine(_, 0, _))        => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _))  => s
            case (Coin, Machine(true, candy, coin)) =>
              Machine(locked = false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
              Machine(locked = true, candy - 1, coin)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      for {
        _ <- xsequence(inputs map (modify[Machine] _ compose update))
        s <- get
      } yield (s.coins, s.candies)

    def _simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      for {
        _ <- xsequence(inputs map (i => modify[Machine](update(i))))
        s <- get
      } yield (s.coins, s.candies)
  }
}
