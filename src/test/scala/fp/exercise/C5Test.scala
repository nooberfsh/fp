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
}
