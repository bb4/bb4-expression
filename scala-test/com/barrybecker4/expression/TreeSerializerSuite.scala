package com.barrybecker4.expression

import com.barrybecker4.expression.mathexpression.{MathExpressionParser, MathOperatorsDefinition, TIMES, PLUS, EXPONENT}
import com.barrybecker4.testsupport.strip
import org.scalatest.Assertions.assertResult
import org.scalatest.funsuite.AnyFunSuite

class TreeSerializerSuite extends AnyFunSuite {

  val serializer = new TreeSerializer

  test("regular serialize null node yields empty string") {
    assertResult("")(serializer.regularSerialize(null))
  }

  test("indented serialize null node yields empty string") {
    assertResult("")(serializer.indentedSerialize(null))
  }

  test("regular serialize expression") {
    val expr = BinOp(TIMES, X, Const(3))
    assertResult("x * 3.0") {
      serializer.regularSerialize(expr)
    }
  }

  test("serialize adds parentheses when a looser operator is nested under a tighter one") {
    // x ^ (-1 * x) must not print as x ^ -1 * x (that re-parses as (x ^ -1) * x)
    val expr = BinOp(EXPONENT, X, BinOp(TIMES, Const(-1.0), X))
    assertResult("x ^ (-1.0 * x)") {
      serializer.regularSerialize(expr)
    }
  }

  test("serialize Const(-1) like other integers") {
    assertResult("-1.0") {
      serializer.regularSerialize(Const(-1.0))
    }
  }

  test("indented serialize simple expression") {
    val inner = BinOp(TIMES, X, Const(3))
    val root = BinOp(PLUS, X, inner)

    assertResult("x + x * 3.0") {
      serializer.indentedSerialize(root)
    }
  }

  test("indented serialize medium expression") {

    val str = strip("""1 + ((x + 4) / (x^2 - 1)) / (2x + 4)""", "")

    val opDef = new MathOperatorsDefinition
    val parser = new MathExpressionParser(opDef)
    val root = parser.parse(str)
    val serialized = serializer.indentedSerialize(root)

    val expected =
      strip("""1.0 + (
        |    (
        |      x + 4.0
        |    ) / (
        |      x ^ 2.0 - 1.0
        |    )
        |  ) / (
        |    2.0 * x + 4.0
        |  )""")

    assertResult(expected) { strip(serialized) }
  }
}
