package com.barrybecker4.expression

import com.barrybecker4.expression.mathexpression.{MathExpressionParser, PLUS, TIMES}
import org.scalatest.funsuite.AnyFunSuite

class ExprEvaluatorSuite extends AnyFunSuite {

  private val parser = new MathExpressionParser()

  test("eval uses Operator.operate for parsed tree") {
    val expr = parser.parse("2*x+1")
    assert(5.0 === ExprEvaluator.eval(expr, 2.0))
  }

  test("eval unwraps Group") {
    val expr = BinOp(PLUS, Group(Const(1.0)), Const(2.0))
    assert(3.0 === ExprEvaluator.eval(expr, 99.0))
  }

  test("eval BinOp directly") {
    val e = BinOp(TIMES, Const(3.0), X)
    assert(12.0 === ExprEvaluator.eval(e, 4.0))
  }

  test("eval exponent via Operator.operate uses math.pow (tolerance)") {
    val expr = parser.parse("x^3")
    val actual = ExprEvaluator.eval(expr, 2.0)
    assert(Math.abs(8.0 - actual) < 1e-9)
  }

  test("eval nested expression with mixed ops") {
    // (2*x+1)^2 at x=3 → 49
    val expr = parser.parse("(2*x+1)^2")
    val actual = ExprEvaluator.eval(expr, 3.0)
    assert(Math.abs(49.0 - actual) < 1e-9)
  }

  test("eval division by zero yields Infinity") {
    val expr = parser.parse("1/x")
    assert(ExprEvaluator.eval(expr, 0.0).isInfinity)
  }
}
