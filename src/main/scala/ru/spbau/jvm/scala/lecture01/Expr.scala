package ru.spbau.jvm.scala.lecture01

trait Expr {
  def eval(): Double
}

class Number(value: Double) extends Expr {
  override def eval(): Double = value
}

class Bop(opType: String, left: Expr, right: Expr) extends Expr {
  override def eval(): Double = opType match {
      case "+" => left.eval() + right.eval()
      case "-" => left.eval() - right.eval()
      case "*" => left.eval() * right.eval()
      case "/" => left.eval() / right.eval()
      case "^" => scala.math.pow(left.eval(), right.eval())
    }
}