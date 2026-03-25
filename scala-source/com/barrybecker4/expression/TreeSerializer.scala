/* Copyright by Barry G. Becker, 2000-2019. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Turns a tree into a string via in order traversal.
  * Implements visitor pattern
  * @author Barry Becker
  */
class TreeSerializer {

  def regularSerialize(node: TreeNode): String =
    serialize(node, indented = false)

  def indentedSerialize(node: TreeNode): String =
    serialize(node, indented = true)

  private def serialize(node: TreeNode, indented: Boolean): String =
    if node == null then ""
    else
      val sb = new StringBuilder
      walk(node, depth = 0, indented, sb)
      val s = sb.toString
      if s.nonEmpty then s else "Invalid"

  /** In-order walk: `indented` controls newlines and spacing inside parenthesized subtrees. */
  private def walk(node: TreeNode, depth: Int, indented: Boolean, sb: StringBuilder): Unit =
    if node.children.size == 2 then
      val hp = node.hasParens
      if indented && hp then
        sb.append("(\n").append("  " * depth)
        walk(node.children.head, depth + 1, indented, sb)
        sb.append(" ").append(node.getData).append(" ")
        walk(node.children(1), depth + 1, indented, sb)
        sb.append("\n").append("  " * (depth - 1)).append(LexToken.RightParen.symbol)
      else if hp then
        sb.append(LexToken.LeftParen.symbol)
        walk(node.children.head, depth + 1, indented, sb)
        sb.append(" ").append(node.getData).append(" ")
        walk(node.children(1), depth + 1, indented, sb)
        sb.append(LexToken.RightParen.symbol)
      else
        walk(node.children.head, depth + 1, indented, sb)
        sb.append(" ").append(node.getData).append(" ")
        walk(node.children(1), depth + 1, indented, sb)
    else sb.append(node.getData)

}
