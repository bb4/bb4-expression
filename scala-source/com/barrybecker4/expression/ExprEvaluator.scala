/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Evaluates a parsed [[Expr]] for a concrete value of `x` using [[Operator.operate]].
  */
object ExprEvaluator {

  def eval(expr: Expr, x: Double): Double =
    expr match
      case X                 => x
      case Const(v)          => v
      case BinOp(op, l, r)   => op.operate(eval(l, x), eval(r, x))
      case Group(inner)      => eval(inner, x)
}
