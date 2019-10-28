package fp.exercise

import org.scalatest.{FunSuite, DiagrammedAssertions}
import fp.exercise.C6._

class C6Test extends FunSuite with DiagrammedAssertions {

  case class MockState(num: Int)

  object MockState {
    def incr: Action = State { m =>
      (m.num + 1, MockState(m.num + 1))
    }
    def double: Action = State { m =>
      (m.num * 2, MockState(m.num * 2))
    }
    def add(n: Int): Action = State { m =>
      (m.num + n, MockState(m.num + n))
    }
  }

  type Action = State[MockState, Int]

  ///////////////// test object State

  test("unit") {
    val init = MockState(2)
    val unit = State.unit[MockState, Int](10)
    val (a, s) = unit(init)
    assert(a === 10)
    assert(s.num === 2)
  }

  test("get") {
    val init = MockState(2)
    val get = State.get[MockState]
    val (s1, s2) = get(init)
    assert(s1 === s2)
    assert(s1.num === 2)
  }

  test("set") {
    val before = MockState(2)
    val after = MockState(3)
    val set = State.set[MockState](after)
    val (a, s) = set(before)
    assert(a === ())
    assert(s.num === 3)
  }

  test("modify") {
    val init = MockState(2)
    val f = (m: MockState) => MockState(m.num * 2)
    val modify = State.modify(f)
    val (a, s) = modify(init)
    assert(a === ())
    assert(s.num === 4)

    val _modify = State._modify(f)
    val (a2, s2) = _modify(init)
    assert(a2 === ())
    assert(s2.num === 4)
  }

  test("sequence") {
    val init = MockState(2)
    val actions = List(MockState.incr, MockState.double)
    val sequence = State.sequence(actions)
    val (a, s) = sequence(init)
    assert(a == List(3, 6))
    assert(s.num === 6)
  }

  test("_sequence") {
    val init = MockState(2)
    val actions = List(MockState.incr, MockState.double)
    val sequence = State._sequence(actions)
    val (a, s) = sequence(init)
    assert(a == List(3, 6))
    assert(s.num === 6)
  }

  test("sequenceReverse") {
    val init = MockState(2)
    val actions = List(MockState.incr, MockState.double)
    val sequenceReverse = State.sequenceReverse(actions)
    val (a, s) = sequenceReverse(init)
    assert(a === List(5, 4))
    assert(s.num === 5)
  }

  ///////////////// test case class State

  test("map") {
    val init = MockState(2)
    val f = (a: Int) => a - 1
    val (a, s) = MockState.incr.map(f)(init)
    assert(a === 2)
    assert(s.num === 3)

    val (a2, s2) = MockState.incr._map(f)(init)
    assert(a2 === 2)
    assert(s2.num === 3)
  }

  test("map2") {
    val init = MockState(2)
    val f = (a: Int, b: Int) => a * b
    val (a, s) = MockState.incr.map2(MockState.double)(f)(init)
    assert(a === 18)
    assert(s.num === 6)

    val (a2, s2) = MockState.incr._map2(MockState.double)(f)(init)
    assert(a2 === 18)
    assert(s2.num === 6)
  }

  test("flatMap") {
    val init = MockState(2)
    val f = (a: Int) => MockState.add(a)
    val (a, s) = MockState.incr.flatMap(f)(init)
    assert(a === 6)
    assert(s.num === 6)
  }

  test("join") {
    val init = MockState(2)
    val ((a, b), s) = MockState.incr.join(MockState.double)(init)
    assert(a === 3)
    assert(b === 6)
    assert(s.num == 6)
  }

}
