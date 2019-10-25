package fp.exercise

import org.scalatest.{FunSuite, DiagrammedAssertions}
import C3._

class C3Test extends FunSuite with DiagrammedAssertions {
  //////////////////////////////////////////////////////////////////////////////
  //                     list test
  //////////////////////////////////////////////////////////////////////////////


  //////////////////////////////////////////////////////////////////////////////
  //                     tree test
  //////////////////////////////////////////////////////////////////////////////

  test("tree size") {
    val t1: Tree[Int] = Leaf(1)
    assert(t1.size === 1)

    val t2 = Leaf(2)
    val t12 = Branch(t1, t2)
    val t3 = Leaf(3)
    val t4 = Branch(t12, t3)
    assert(t4.size === 5)
  }

  test("tree depth") {
    val t1: Tree[Int] = Leaf(1)
    assert(t1.depth === 1)

    val t2 = Leaf(2)
    val t12 = Branch(t1, t2)
    val t3 = Leaf(3)
    val t4 = Branch(t12, t3)
    assert(t4.depth === 3)
  }

  test("tree map") {
    val t1: Tree[Int] = Leaf(1)
    val t1M = t1.map(_.toString)
    assert(t1M === Leaf("1"))

    val t2 = Leaf(2)
    val t12 = Branch(t1, t2)
    val t3 = Leaf(3)
    val t4 = Branch(t12, t3)
    val t4M = t4.map(_.toString)
    assert(t4M === Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")))
  }
}
