package com.barrybecker4.expression

import com.barrybecker4.expression.mathexpression.MathExpressionParser
import org.scalatest.funsuite.AnyFunSuite

class ExpressionSuite extends AnyFunSuite {

  private val parser = new MathExpressionParser()

  test("Expression captures successful parse") {
    val ex = Expression("x", parser)
    assert(ex.isValid)
    assert(ex.rootNode.isDefined)
    assert(ex.parseFailure.isEmpty)
  }

  test("Expression captures parse failure without throwing from factory") {
    val ex = Expression("(", parser)
    assert(!ex.isValid)
    assert(ex.rootNode.isEmpty)
    assert(ex.parseFailure.isDefined)
  }
}
