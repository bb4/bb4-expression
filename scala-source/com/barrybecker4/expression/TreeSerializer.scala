/* Copyright by Barry G. Becker, 2000-2019. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Turns a tree into a string via in order traversal.
  * Implements visitor pattern
  * @author Barry Becker
  */
class TreeSerializer {

  def regularSerialize(node: TreeNode): String = {
    serialize(node, traverse)
  }

  def indentedSerialize(node: TreeNode): String = {
    serialize(node, indentedTraverse)
  }

  private def serialize(node: TreeNode, traverser: (TreeNode, Int) => String): String = {
    var serialized = ""
    if (node != null) serialized = traverser(node, 0)
    if (serialized.length > 0) serialized
    else "Invalid"
  }

  /** processing for inner nodes */
  private def traverse(node: TreeNode, depth: Int): String = {
    var text = ""
    if (node.children.size == 2) {
      val lsymb = if (node.hasParens) LEFT_PAREN.symbol else ""
      val rsymb = if (node.hasParens) RIGHT_PAREN.symbol else ""
      text += s"$lsymb${traverse(node.children.head, depth + 1)}"
      text += s" ${node.getData} "
      text += traverse(node.children(1), depth + 1) + rsymb
    }
    else text += node.getData
    text
  }

  private def indentedTraverse(node: TreeNode, depth: Int): String = {
    var text = ""
    if (node.children.size == 2) {
      val indent = if (node.hasParens) "  " * depth else ""
      val lsymb = if (node.hasParens) s"${LEFT_PAREN.symbol}\n" else ""
      val rsymb = if (node.hasParens) s"\n${"  " * (depth - 1)}${RIGHT_PAREN.symbol}" else ""

      text += s"$lsymb$indent${indentedTraverse(node.children.head, depth + 1)}"
      text += s" ${node.getData} "
      text += indentedTraverse(node.children(1), depth + 1) + rsymb
    }
    else text += node.getData
    text
  }
}