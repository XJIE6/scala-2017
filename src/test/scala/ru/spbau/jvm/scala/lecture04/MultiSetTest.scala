package ru.spbau.jvm.scala.lecture04

import org.scalatest.FunSuite

class MultiSetTest extends FunSuite {
  test("Add") {
    assert(Nil + "3" == Cons("3", 1, Nil))
    assert(Nil + "3" + "3" == Cons("3", 2, Nil))
    assert(Nil + "3" + "1" + "3" == Cons("3", 2, Cons("1", 1, Nil)))
  }

  val m: MultiSet[String] = Cons("1", 1, Cons("2", 2, Nil))

  test("Find") {
    assert(m("1") == 1)
    assert(m("123") == 0)
    assert(m("2") == 2)
  }

  test("match") {
    assert((m match {
      case MultiSet("1") => 1
      case Nil => 2
      case MultiSet("1", "2", "2") => 3
      case Cons(_, _, _) => 4
    }) == 3)
  }

  test("for"){
    var seq: Seq[String] = Seq.empty[String]
    for (e <- m) {
      seq = seq :+ e
    }
    assert(MultiSet.unapplySeq(m).contains(seq))
  }

}
