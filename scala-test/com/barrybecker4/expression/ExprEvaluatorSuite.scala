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
}
