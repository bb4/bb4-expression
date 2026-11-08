/*
 * Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT
 */
package com.barrybecker4.expression.mathexpression

import com.barrybecker4.expression.{ExprEvaluator, ParseError, TreeSerializer}
import com.barrybecker4.testsupport.strip
import org.scalatest.Assertions
import org.scalatest.funsuite.AnyFunSuite


/**
  * Ported from flex project.
  * @author Barry Becker
  */
class MathExpressionParserSuite extends AnyFunSuite {
  val opDef = new MathOperatorsDefinition
  /** instance under test */
  private val parser = new MathExpressionParser(opDef)
  /** used to verify parsed tree */
  private val serializer = new TreeSerializer


  test("XOnlyExp")  {
    verifyParse("x")
  }

  test("NegXExp") {
    verifyParse("-x", "-1.0 * x")
  }

  test("ConstantIntOnlyExp(") {
    verifyParse("5", "5.0")
  }

  test("NegativeConstantIntExp(") {
    verifyParse("-5", "-5.0")
  }

  test("NegativeConstantDecimalExp(") {
    verifyParse("-5.3")
  }

  test("ScaledXExp(") {
    verifyParse("2.3x", "2.3 * x")
  }

  test("ScaledXExpWithSpaces(") {
    verifyParse("2.3x", "2.3 * x")
  }

  test("XsquaredExp") {
    verifyParse("x*x", "x * x")
  }

  test("XtimesNegXExp") {
    // -x is one operand, so the tree is x * (-1 * x), not the flat (x * -1) * x
    verifyParse("x*-x", "x * (-1.0 * x)")
  }

  test("XRaisedToXMinus3XExp") {
    verifyParse("x^x-3x", "x ^ x - 3.0 * x")
  }

  /** Serialization alone cannot tell ((x^-1)*x)-3x from (x^(-x))-3x; assert evaluation. */
  test("XRaisedToNegXMinus3XExp") {
    val expr = parser.parse("x^-x-3x")
    assertResult("x ^ (-1.0 * x) - 3.0 * x") {
      serializer.regularSerialize(expr)
    }
    assert(math.abs(-5.75 - ExprEvaluator.eval(expr, 2.0)) < 1e-9)
  }

  test("unary minus stays inside the right operand of ^ and /") {
    assert(math.abs(0.25 - ExprEvaluator.eval(parser.parse("x^-x"), 2.0)) < 1e-9)
    assert(-1.0 === ExprEvaluator.eval(parser.parse("x/-x"), 4.0))
    assert(math.abs(0.0625 - ExprEvaluator.eval(parser.parse("2^-x"), 4.0)) < 1e-9)
  }

  test("unary minus of a parenthesized group") {
    assert(-5.0 === ExprEvaluator.eval(parser.parse("-(x+1)"), 4.0))
  }

  test("double minus on a literal still parses") {
    assert(5.0 === ExprEvaluator.eval(parser.parse("2--3"), 0.0))
  }

  test("-2^2 and -x^2 follow the same unary precedence (EBNF: unary tighter than ^)") {
    val lit = ExprEvaluator.eval(parser.parse("-2^2"), 0.0)
    val vari = ExprEvaluator.eval(parser.parse("-x^2"), 2.0)
    assert(4.0 === lit)
    assert(4.0 === vari)
  }

  test("negative coefficient times x is one operand") {
    assert(-6.0 === ExprEvaluator.eval(parser.parse("-2x"), 3.0))
    verifyParse("-2x", "-2.0 * x")
  }

  test("positive coefficient times x leaves ^ tighter than *") {
    // 2x^2 must be 2*(x^2), not (2x)^2 — only unary-minus terms are pre-grouped
    assert(18.0 === ExprEvaluator.eval(parser.parse("2x^2"), 3.0))
    verifyParse("2x^2", "2.0 * x ^ 2.0")
  }

  test("XXXExp") {
    verifyParse("x*x *  x", "x * x * x")
  }

  test("XCubedMinusXCubedExp") {
    verifyParse("x^3 - x^3", "x ^ 3.0 - x ^ 3.0")
  }

  test("ParenExpMinus3") {
    verifyParse("(2x + 1) - 3", "(2.0 * x + 1.0) - 3.0")
  }

  test("3MinusParenExp") {
    verifyParse(strip("3 - (2x + 1)"), "3.0 - (2.0 * x + 1.0)")
  }

  test("XMinus2") {
    verifyParse("x - 2", "x - 2.0")
  }

  test("2MinusX") {
    verifyParse("2 - x", "2.0 - x")
  }

  test("5X") {
    verifyParse("5x", "5.0 * x")
  }

  test("multiplication binds tighter than addition") {
    verifyParse("2 + 3 * 4", "2.0 + 3.0 * 4.0")
  }

  test("parse rejects null expression") {
    Assertions.intercept[IllegalArgumentException](parser.parse(null))
  }

  test("parse rejects empty expression") {
    Assertions.intercept[IllegalArgumentException](parser.parse(""))
  }

  test("parse rejects whitespace-only expression") {
    Assertions.intercept[IllegalArgumentException](parser.parse("   \t  "))
  }

  test("parse rejects mismatched parenthesis") {
    val ex = Assertions.intercept[ParseError](parser.parse("(x+1"))
    assert(ex.getMessage.contains("Mismatched"))
  }

  test("mixed exponent subtraction and multiplication") {
    verifyParse("x ^ x - 3 * x", "x ^ x - 3.0 * x")
  }

  test("ComplexNestedExp") {
    verifyParse(strip("""
        |(1 + ((x + 4) / (x^2 - 1)) / ((2x + 4) / (x^2 - 1))   +
        |(x + ((x + 4) / (x^2 - 1)) / ((2x + 4) / (x^2 - 1)))) *
        |(1 + ((x + 4) / (x^2 - 1)) / ((2x + 4) / (x^2 - 1))   +
        |(x + ((x + 4) / (x^2 - 1)) / ((2x + 4) / (x^2 - 1))))""", ""),
      strip("""
        |(1.0 + ((x + 4.0) / (x ^ 2.0 - 1.0)) / ((2.0 * x + 4.0) / (x ^ 2.0 - 1.0)) +
        | (x + ((x + 4.0) / (x ^ 2.0 - 1.0)) / ((2.0 * x + 4.0) / (x ^ 2.0 - 1.0)))) *
        | (1.0 + ((x + 4.0) / (x ^ 2.0 - 1.0)) / ((2.0 * x + 4.0) / (x ^ 2.0 - 1.0)) +
        | (x + ((x + 4.0) / (x ^ 2.0 - 1.0)) / ((2.0 * x + 4.0) / (x ^ 2.0 - 1.0))))""", "")
    )
  }

  /**
    * @param exp the expression to parse
    */
  private def verifyParse(exp: String): Unit = {
    verifyParse(exp, exp)
  }

  private def verifyParse(exp: String, expSerializedStr: String): Unit = {
    val root = parser.parse(exp)
    val serialized = serializer.regularSerialize(root)
    assertResult(expSerializedStr) { serialized }
  }
}
