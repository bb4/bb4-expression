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

  test("eval same-operator chains use left-associativity") {
    assert(1.0 === ExprEvaluator.eval(parser.parse("8/4/2"), 0.0))
    assert(5.0 === ExprEvaluator.eval(parser.parse("10-3-2"), 0.0))
    val pow = ExprEvaluator.eval(parser.parse("2^3^2"), 0.0)
    assert(Math.abs(64.0 - pow) < 1e-9)
  }

  test("eval division by zero yields Infinity") {
    val expr = parser.parse("1/x")
    assert(ExprEvaluator.eval(expr, 0.0).isInfinity)
  }

  test("juxtaposition binds tighter than explicit division, but not tighter than ^") {
    assert(math.abs(2.0 / 3.0 - ExprEvaluator.eval(parser.parse("2x/3x"), 3.0)) < 1e-9)
    assert(0.5 === ExprEvaluator.eval(parser.parse("x/2x"), 3.0))
    assert(math.abs(2.0 / 3.0 - ExprEvaluator.eval(parser.parse("2(x+1)/3(x+1)"), 3.0)) < 1e-9)
    assert(math.abs(18.0 - ExprEvaluator.eval(parser.parse("2x^2"), 3.0)) < 1e-9) // not (2x)^2
  }

  test("explicit * and / stay left-associative") {
    assert(6.0 === ExprEvaluator.eval(parser.parse("2*x/3*x"), 3.0))
  }
}
