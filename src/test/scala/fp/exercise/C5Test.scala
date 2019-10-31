package fp.exercise

import org.scalatest.{FunSuite, DiagrammedAssertions}
import fp.exercise.C5._

class C5Test extends FunSuite with DiagrammedAssertions {
  test("toList") {
    val s = Stream(1, 2, 3, 4)
    val l = s.toList
    assert(l === List(1, 2, 3, 4))
  }

  test("take") {
    val s = Stream(1, 2, 3, 4)
    assert(s.take(2).toList === List(1, 2))
    assert(s.take(0).toList === List())
    assert(s.take(6).toList === List(1, 2, 3, 4))
  }

  test("drop") {
    val s = Stream(1, 2, 3, 4)
    assert(s.drop(2).toList === List(3, 4))
    assert(s.drop(0).toList === List(1, 2, 3, 4))
    assert(s.drop(6).toList === List())
  }

  test("takeWhile") {
    val s = Stream(1, 2, 3, 4)
    val f1 = (i: Int) => i < 3
    val f2 = (i: Int) => i > 5
    assert(s.takeWhile(f1).toList === List(1, 2))
    assert(s.takeWhile(f2).toList === List())
  }

  //////////////////////////////////////////////////////////////////////////////
  test("constant") {
    val s = Stream.constant(2)
    assert(s.take(2).toList === List(2, 2))
    assert(s.take(1).toList === List(2))
    assert(s.take(0).toList === List())
  }

  test("from") {
    val s = Stream.from(2)
    assert(s.take(2).toList === List(2, 3))
    assert(s.take(1).toList === List(2))
    assert(s.take(0).toList === List())
  }

  test("zip") {
    val s1 = Stream(1, 2, 3)
    val s2 = Stream(10, 11, 12, 14)

    val s = Stream.zip(s1, s2)(_ + _)
    assert(s.toList === List(11, 13, 15))
  }

  test("fibs") {
    val fib = Stream.fibs.take(8)
    assert(fib.toList === List(0, 1, 1, 2, 3, 5, 8, 13))
  }
}
