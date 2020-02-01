package com.barrybecker4.expression

import com.barrybecker4.expression.TreeSerializer
import com.barrybecker4.expression.mathexpression.MathOperatorsDefinition
import org.scalatest.FunSuite

class TreeSerializerSuite extends FunSuite {

  val serializer = new TreeSerializer

  test("serialize expression") {
    val opDef = new MathOperatorsDefinition
    val node: TreeNode = new TreeNode("*", opDef)
    node.children = Seq(new TreeNode("x", opDef), new TreeNode("3", opDef))

    assertResult("x * 3") {
      serializer.serialize(node)
    }
  }
}
