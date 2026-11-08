/*
 * Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT
 */
package com.barrybecker4.expression.mathexpression

import com.barrybecker4.expression.{BinOp, Const, ExprEvaluator, ParseError, TreeSerializer}
import com.barrybecker4.testsupport.strip
import org.scalatest.Assertions
import org.scalatest.funsuite.AnyFunSuite

/**
  * Additional coverage: precedence chains, parens, implicit multiply, and malformed input
  * intended to catch regressions in tokenization and reduction.
  */
class MathExpressionParserExtendedSuite extends AnyFunSuite {

  private val opDef = new MathOperatorsDefinition
  private val parser = new MathExpressionParser(opDef)
  private val serializer = new TreeSerializer

  private def verify(exp: String, expectedSerialized: String): Unit = {
    val root = parser.parse(exp)
    assertResult(expectedSerialized) {
      serializer.regularSerialize(root)
    }
  }

  private def evalAt(exp: String, x: Double = 0.0): Double =
    ExprEvaluator.eval(parser.parse(exp), x)

  // --- Precedence and associativity (value and tree shape; serialization alone is insufficient) ---

  test("division chains are left-associative") {
    verify("8/4/2", "8.0 / 4.0 / 2.0")
    // right-associative reduction would evaluate to 4
    assert(1.0 === evalAt("8/4/2"))
    parser.parse("8/4/2") match {
      case BinOp(DIVIDE, BinOp(DIVIDE, Const(a), Const(b)), Const(c)) =>
        assert(a === 8.0 && b === 4.0 && c === 2.0)
      case other => fail(other.toString)
    }
  }

  test("multiplication chains are left-associative") {
    verify("2*3*4", "2.0 * 3.0 * 4.0")
    assert(24.0 === evalAt("2*3*4"))
    parser.parse("2*3*4") match {
      case BinOp(TIMES, BinOp(TIMES, Const(a), Const(b)), Const(c)) =>
        assert(a === 2.0 && b === 3.0 && c === 4.0)
      case other => fail(other.toString)
    }
  }

  test("subtraction chains are left-associative") {
    verify("10-3-2", "10.0 - 3.0 - 2.0")
    // right-associative reduction would evaluate to 9
    assert(5.0 === evalAt("10-3-2"))
  }

  test("addition chains are left-associative") {
    verify("1+2+3", "1.0 + 2.0 + 3.0")
    assert(6.0 === evalAt("1+2+3"))
    parser.parse("1+2+3") match {
      case BinOp(PLUS, BinOp(PLUS, Const(a), Const(b)), Const(c)) =>
        assert(a === 1.0 && b === 2.0 && c === 3.0)
      case other => fail(other.toString)
    }
  }

  test("exponent towers use left-associativity in reducer") {
    verify("2^3^2", "2.0 ^ 3.0 ^ 2.0")
    // right-associative would be 512; left-associative is the documented choice
    val pow = evalAt("2^3^2")
    assert(math.abs(64.0 - pow) < 1e-9)
  }

  test("division binds tighter than subtraction") {
    verify("10-8/2", "10.0 - 8.0 / 2.0")
    // (10-8)/2 would be 1
    assert(6.0 === evalAt("10-8/2"))
  }

  test("multiplication binds tighter than subtraction") {
    verify("10-2*3", "10.0 - 2.0 * 3.0")
    // (10-2)*3 would be 24
    assert(4.0 === evalAt("10-2*3"))
  }

  test("exponent binds tighter than multiplication") {
    verify("x^2*3", "x ^ 2.0 * 3.0")
    // (x^(2*3)) at x=2 would be 64
    val e = evalAt("x^2*3", 2.0)
    assert(math.abs(12.0 - e) < 1e-9)
  }

  test("parentheses force addition before multiplication") {
    verify("(2+3)*4", "(2.0 + 3.0) * 4.0")
  }

  test("parentheses inside right operand of multiplication") {
    verify("2*(3+4)", "2.0 * (3.0 + 4.0)")
  }

  test("nested parens with division and powers") {
    verify("((x+1)^2)/(x-1)", "((x + 1.0) ^ 2.0) / (x - 1.0)")
  }

  // --- Implicit multiplication ---

  test("numeric coefficient before parenthesized subexpression") {
    verify("2(x+1)", "2.0 * (x + 1.0)")
  }

  test("coefficient x before parenthesized subexpression") {
    verify("x(x+1)", "x * (x + 1.0)")
  }

  test("adjacent to closing paren then x uses implicit multiply") {
    verify("(x+1)x", "(x + 1.0) * x")
  }

  test("adjacent to closing paren then digit uses implicit multiply") {
    verify("(x+1)2", "(x + 1.0) * 2.0")
  }

  test("two parenthesized groups juxtaposed use implicit multiply") {
    verify("(x+1)(x-1)", "(x + 1.0) * (x - 1.0)")
  }

  test("binary minus after closed parenthesis group") {
    verify("(x+1)-2", "(x + 1.0) - 2.0")
  }

  // --- Normalization / whitespace ---

  test("leading and trailing spaces are trimmed") {
    verify("  x + 1  ", "x + 1.0")
  }

  test("internal spaces are ignored") {
    verify("1  +  2  *  3", "1.0 + 2.0 * 3.0")
  }

  test("space does not glue digits into one literal") {
    val ex = Assertions.intercept[ParseError](parser.parse("2 3"))
    assert(ex.offset > 0)
  }

  test("space does not glue decimal fragments into one literal") {
    Assertions.intercept[ParseError](parser.parse("2 . 3"))
  }

  test("internal tabs are whitespace, same as spaces") {
    verify("1\t+\t2", "1.0 + 2.0")
  }

  test("space before a parenthesized group is still implicit multiplication") {
    assert(6.0 === ExprEvaluator.eval(parser.parse("2 (3)"), 0.0))
  }

  test("space before x is still implicit multiplication") {
    verify("2 x", "2.0 * x")
  }

  // --- Deep structure ---

  test("triply nested parens around x still parse") {
    verify("(((x)))", "x")
  }

  test("wide expression with mixed ops") {
    verify(
      strip("""(x+1)*(x-1)/(x^2+1) + 2*x/(x+3)"""),
      strip("""(x + 1.0) * (x - 1.0) / (x ^ 2.0 + 1.0) + 2.0 * x / (x + 3.0)""")
    )
  }

  // --- Malformed input (regression guards) ---

  test("reject digit immediately after x in a term") {
    val ex = Assertions.intercept[IllegalArgumentException](parser.parse("x2"))
    assert(ex.getMessage.contains("numbers after x"))
  }

  test("reject digit after x when term began with coefficient") {
    val ex = Assertions.intercept[IllegalArgumentException](parser.parse("2x3"))
    assert(ex.getMessage.contains("numbers after x"))
  }

  test("reject empty parenthesized subexpression") {
    Assertions.intercept[IllegalArgumentException](parser.parse("()"))
  }

  test("reject unknown variable letter") {
    val ex = Assertions.intercept[ParseError](parser.parse("y+1"))
    assert(ex.getMessage.contains("Unexpected character"))
    assertResult(0)(ex.offset)
  }

  test("reject extra closing parenthesis") {
    Assertions.intercept[IllegalArgumentException](parser.parse("(x+1))"))
  }

  test("reject single binary operator") {
    Assertions.intercept[IllegalArgumentException](parser.parse("+"))
  }

  test("reject trailing binary operator") {
    Assertions.intercept[IllegalArgumentException](parser.parse("1+"))
  }

  test("reject leading binary operator after nullary start would be invalid - lone star") {
    Assertions.intercept[IllegalArgumentException](parser.parse("*3"))
  }

  test("reject consecutive binary operators with no operand between") {
    Assertions.intercept[IllegalArgumentException](parser.parse("1+*2"))
  }

  test("reject malformed decimal with adjacent dots") {
    Assertions.intercept[IllegalArgumentException | NumberFormatException] {
      parser.parse("1..2")
    }
  }

  test("findClosingParen: early exit on open brace without close in nested scan") {
    Assertions.intercept[ParseError](parser.parse("((x+1)"))
  }

  // --- parse == parse (idempotent string round-trip shape) ---

  test("re-serializing complex parse does not change on second parse") {
    val input =
      strip("""(1+x)*(x^2-3.5)/(x+2) + (x-1)*((x+4)/(x-2))""")
    val once = serializer.regularSerialize(parser.parse(input))
    val twice = serializer.regularSerialize(parser.parse(once))
    assertResult(once)(twice)
  }

  test("scientific notation from Double.toString round-trips") {
    for (src <- Seq("10000000", "0.0001", "x+10000000", "x+0.0001")) {
      val once = serializer.regularSerialize(parser.parse(src))
      val twice = serializer.regularSerialize(parser.parse(once))
      assert(once === twice)
    }
  }

  test("a literal past Double range is a parse error, not Infinity") {
    val huge = "1" + ("0" * 400)
    val ex = intercept[ParseError](parser.parse(huge))
    assert(ex.getMessage.toLowerCase.contains("infinite") || ex.getMessage.contains(huge.take(8)))
  }

  test("explicit exponent is accepted when the lexer allows it") {
    assert(1.0e7 === ExprEvaluator.eval(parser.parse("1.0E7"), 0.0))
    assert(1.0e-4 === ExprEvaluator.eval(parser.parse("1E-4"), 0.0))
  }

  test("bare e remains an unexpected character") {
    val ex = intercept[ParseError](parser.parse("e"))
    assert(ex.getMessage.contains("Unexpected character"))
  }
}
