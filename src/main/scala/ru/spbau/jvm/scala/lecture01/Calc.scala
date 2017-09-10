package ru.spbau.jvm.scala.lecture01

object Calc {
  def eval(in:String):Double = {
    Parser.apply(in).eval()
  }
}
