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

  //////////////////////////////////////////////////////////////////////////////
  // unfold

  test("_constant") {
    val s = Stream._constant(2)
    assert(s.take(2).toList === List(2, 2))
    assert(s.take(1).toList === List(2))
    assert(s.take(0).toList === List())
  }

  test("_from") {
    val s = Stream._from(2)
    assert(s.take(2).toList === List(2, 3))
    assert(s.take(1).toList === List(2))
    assert(s.take(0).toList === List())
  }

  test("_fibs") {
    val fib = Stream._fibs.take(8)
    assert(fib.toList === List(0, 1, 1, 2, 3, 5, 8, 13))
  }

  //////////////////////////////////////////////////////////////////////////////

  test("_map") {
    val s = Stream(1, 2)._map(_.toString).toList
    assert(s === List("1", "2"))
  }

  test("_take") {
    val s = Stream(1, 2)
    assert(s._take(1).toList === List(1))
    assert(s._take(3).toList === List(1, 2))
  }

  test("_takeWhile") {
    val s = Stream(1, 2, 3, 4)
    val f1 = (i: Int) => i < 3
    val f2 = (i: Int) => i > 5
    assert(s._takeWhile(f1).toList === List(1, 2))
    assert(s._takeWhile(f2).toList === List())
  }

  test("zipWith") {
    val s = Stream(1, 2)
    val s1 = Stream(1)
    val s2 = Stream(4, 5, 6)

    assert(s.zipWith(s1).toList === List((1, 1)))
    assert(s.zipWith(s2).toList === List((1, 4), (2, 5)))
  }

  test("zipAll") {
    val s = Stream(1, 2)
    val s1 = Stream(1)

    assert(s.zipAll(s1).toList === List((Some(1), Some(1)), (Some(2), None)))
    assert(s1.zipAll(s).toList === List((Some(1), Some(1)), (None, Some(2))))
  }

  test("exists") {
    val s = Stream(1, 2)
    val f1 = (a: Int) => a == 1
    val f2 = (a: Int) => a > 3

    assert(s.exists(f1) === true)
    assert(s.exists(f2) === false)
  }

  test("hasSubsequence") {
    val s = Stream(1,2,3)
    val s1 = Stream(1,3)
    val s2 = Stream(2,3)

    assert(s.hasSubsequence(s1) === false)
    assert(s.hasSubsequence(s2) === true)
    assert(s.hasSubsequence(s) === true)

    val a = Stream.empty[Int]
    assert(a.hasSubsequence(a) === true)
  }

  test("startWith") {
    val s = Stream(1, 2, 3)
    val s2 = Stream(1, 2)
    val s3 = Stream(1, 3, 2)

    assert(s.startsWith(s2) === true)
    assert(s.startsWith(s3) === false)

    assert(s._startsWith(s2) === true)
    assert(s._startsWith(s3) === false)
  }

  test("tails") {
    val s = Stream(1, 2, 3)
    val ss = s.tails.map(_.toList).toList
    assert(ss === List(List(1, 2, 3), List(2, 3), List(3), Nil))
  }

  test("scanRight") {
    val l = Stream(1, 2, 3).scanRight(0)(_ + _).toList
    assert(l === List(6, 5, 3, 0))

    val l2 = Stream(1, 2, 3)._scanRight(0)(_ + _).toList
    assert(l2 === List(6, 5, 3, 0))
  }
}
