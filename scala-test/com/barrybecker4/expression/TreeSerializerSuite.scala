package com.barrybecker4.expression

import com.barrybecker4.expression.mathexpression.{MathExpressionParser, MathOperatorsDefinition}
import com.barrybecker4.testsupport.strip
import org.scalatest.Assertions.assertResult
import org.scalatest.funsuite.AnyFunSuite


class TreeSerializerSuite extends AnyFunSuite {

  val serializer = new TreeSerializer

  test("regular serialize expression") {
    val opDef = new MathOperatorsDefinition
    val node: TreeNode = new TreeNode("*", opDef)
    node.children = Seq(new TreeNode("x", opDef), new TreeNode("3", opDef))

    assertResult("x * 3") {
      serializer.regularSerialize(node)
    }
  }

  test("indented serialize simplee xpression") {
    val opDef = new MathOperatorsDefinition
    val node: TreeNode = new TreeNode("*", opDef)
    node.children = Seq(new TreeNode("x", opDef), new TreeNode("3", opDef))

    val root = new TreeNode("+", opDef)
    root.children = Seq(new TreeNode("x", opDef), node)

    assertResult("x + x * 3") {
      serializer.indentedSerialize(root)
    }
  }

  test("indented serialize medium expression") {

    val str = strip("""1 + ((x + 4) / (x^2 - 1)) / (2x + 4)""", "")

    val opDef = new MathOperatorsDefinition
    val parser = new MathExpressionParser(opDef)
    val root = parser.parse(str)
    val serialized = serializer.indentedSerialize(root)
    println("serialized = \n" + serialized)
    println("-----done")

    val expected =
      strip("""1.0 + (
        |    (
        |      x + 4.0
        |    ) / (
        |      x ^ 2.0 - 1.0
        |    )
        |  ) / (
        |  2.0 * x + 4.0
        |)""")

    assertResult(expected) { strip(serialized) }
  }
}
