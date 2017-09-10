package ru.spbau.jvm.scala.lecture01

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def number    : Parser[Expr] = """-?\d+(\.\d*)?""".r ^^ {str => new Number(str.toDouble) }
  def priority0 : Parser[Expr] = number | "(" ~> expr <~ ")"
  def priority1 : Parser[Expr] = priority0 ~ rep("^" ~ priority0) ^^ {
    case number ~ list => (number /: list) {
      case (x, c ~ y) => new Bop(c, x, y)
    }
  }
  def priority2 : Parser[Expr] = priority1 ~ rep("*" ~ priority1 | "/" ~ priority1) ^^ {
    case number ~ list => (number /: list) {
      case (x, c ~ y) => new Bop(c, x, y)
    }
  }
  def expr      : Parser[Expr] = priority2 ~ rep("+" ~ priority2 | "-" ~ priority2) ^^ {
    case number ~ list => (number /: list) {
      case (x, c ~ y) => new Bop(c, x, y)
    }
  }
  def apply(input: String): Expr = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}
