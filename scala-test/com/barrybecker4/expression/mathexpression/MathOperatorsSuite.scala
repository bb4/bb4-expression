/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression.mathexpression

import org.scalatest.Assertions
import org.scalatest.funsuite.AnyFunSuite

/** Characterization tests for [[MathOperators]] enum cases and `operatorFor`. */
class MathOperatorsSuite extends AnyFunSuite {

  test("symbols match historical operator characters") {
    assertResult('+')(PLUS.symbol)
    assertResult('-')(MINUS.symbol)
    assertResult('*')(TIMES.symbol)
    assertResult('/')(DIVIDE.symbol)
    assertResult('^')(EXPONENT.symbol)
  }

  test("operate covers arithmetic cases exactly") {
    assertResult(5.0)(PLUS.operate(2.0, 3.0))
    assertResult(-1.0)(MINUS.operate(2.0, 3.0))
    assertResult(6.0)(TIMES.operate(2.0, 3.0))
    assertResult(2.0)(DIVIDE.operate(6.0, 3.0))
  }

  test("exponentiate uses math.pow (tolerance for JVM variance)") {
    val actual = EXPONENT.operate(2.0, 3.0)
    assert(Math.abs(8.0 - actual) < 1e-9)
  }

  test("operatorFor maps each symbol") {
    assert(MathOperators.operatorFor('+') eq PLUS)
    assert(MathOperators.operatorFor('-') eq MINUS)
    assert(MathOperators.operatorFor('*') eq TIMES)
    assert(MathOperators.operatorFor('/') eq DIVIDE)
    assert(MathOperators.operatorFor('^') eq EXPONENT)
  }

  test("operatorFor rejects unknown symbol") {
    val ex = Assertions.intercept[IllegalArgumentException](MathOperators.operatorFor('?'))
    assert(ex.getMessage.contains("Not an operator symbol"))
  }

  test("package aliases refer to the same enum cases") {
    assert(PLUS eq MathOperators.PLUS)
    assert(MINUS eq MathOperators.MINUS)
    assert(TIMES eq MathOperators.TIMES)
    assert(DIVIDE eq MathOperators.DIVIDE)
    assert(EXPONENT eq MathOperators.EXPONENT)
  }
}
