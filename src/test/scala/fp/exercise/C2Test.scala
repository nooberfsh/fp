package fp.exercise

import org.scalatest.{DiagrammedAssertions, FunSuite}
import C2._

class C2Test extends FunSuite with DiagrammedAssertions {
  test("fib") {
    assert(fib(0) === 0)
    assert(fib(1) === 1)
    assert(fib(2) === 1)
    assert(fib(3) === 2)
    assert(fib(4) === 3)
    assert(fib(5) === 5)
    assert(fib(6) === 8)
  }

  test("isSorted") {
    val compare = (a: Int, b: Int) => a <= b
    assert(isSorted(Array(), compare) === true)
    assert(isSorted(Array(1), compare) === true)

    assert(isSorted(Array(1, 2), compare) === true)
    assert(isSorted(Array(2, 1), compare) === false)

    assert(isSorted(Array(1, 2, 2), compare) === true)
    assert(isSorted(Array(1, 2, 7, 6), compare) === false)
  }

  test("curry") {
    val f = (a: Int, b: Int) => a + b
    val curried = curry(f)
    assert(curried(1)(2) === 3)
  }

  test("uncurry") {
    val f = (a: Int) => (b: Int) => a + b
    val uncurried = uncurry(f)
    assert(uncurried(1, 2) === 3)
  }

  test("compose") {
    val addOne = (a: Int) => a + 1
    val multiTwo = (b: Int) => b * 2

    val composed1 = compose(addOne, multiTwo)
    assert(composed1(10) === 21)

    val composed2 = compose(multiTwo, addOne)
    assert(composed2(10) === 22)
  }
}
