package ru.spbau.jvm.scala.lecture01

class CalcTest extends org.scalatest.FunSuite {
  test("simple") {
    assert(Calc.eval("1") == 1)
  }
  test("brackets") {
    assert(Calc.eval("(((((1)))-1))") == 0)
  }
  test("neg") {
    assert(Calc.eval("-1*-2*-3") == -6)
  }
  test("pow") {
    assert(Calc.eval("0^0") == 1)
  }
  test("div") {
    assert(Calc.eval("1/0") == Double.PositiveInfinity)
  }
  test("mul sum") {
    assert(Calc.eval("1 + 2 * 4") == 9)
  }
  test("sum from 1 to 100") {
    assert(Calc.eval("100 * 101 / 2") == 5050)
  }
}
