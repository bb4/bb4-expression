/* Copyright by Barry G. Becker, 2000-2019. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Renders an [[Expr]] as an infix string via recursive in-order traversal (optionally indented for
  * parenthesized binary subtrees). Use explicit cases on [[Expr]] rather than a separate visitor type.
  */
class TreeSerializer {

  def regularSerialize(node: Expr | Null): String =
    serialize(node, indented = false)

  def indentedSerialize(node: Expr | Null): String =
    serialize(node, indented = true)

  private def serialize(node: Expr | Null, indented: Boolean): String =
    if node == null then ""
    else
      val sb = new StringBuilder
      walk(node, depth = 0, indented, sb)
      val s = sb.toString
      if s.nonEmpty then s else "Invalid"

  /** In-order walk: `indented` controls newlines and spacing inside parenthesized subtrees. */
  private def walk(e: Expr, depth: Int, indented: Boolean, sb: StringBuilder): Unit =
    e match
      case BinOp(op, left, right) =>
        emitBinary(left, op, right, hasParens = false, depth, indented, sb)
      case Group(inner) =>
        inner match
          case BinOp(op, left, right) =>
            emitBinary(left, op, right, hasParens = true, depth, indented, sb)
          case other if isAtomicForStrip(other) =>
            walk(other, depth, indented, sb)
          case other =>
            sb.append(LexToken.LeftParen.symbol)
            walk(other, depth + 1, indented, sb)
            sb.append(LexToken.RightParen.symbol)
      case X =>
        sb.append("x")
      case Const(v) =>
        sb.append(
          if v == -1.0 then "-1"
          else constString(v)
        )

  private def emitBinary(left: Expr, op: Operator, right: Expr, hasParens: Boolean, depth: Int, indented: Boolean, sb: StringBuilder): Unit =
    if indented && hasParens then
      sb.append("(\n").append("  " * depth)
      walk(left, depth + 1, indented, sb)
      sb.append(" ").append(op.symbol).append(" ")
      walk(right, depth + 1, indented, sb)
      sb.append("\n").append("  " * (depth - 1)).append(LexToken.RightParen.symbol)
    else if hasParens then
      sb.append(LexToken.LeftParen.symbol)
      walk(left, depth + 1, indented, sb)
      sb.append(" ").append(op.symbol).append(" ")
      walk(right, depth + 1, indented, sb)
      sb.append(LexToken.RightParen.symbol)
    else
      walk(left, depth + 1, indented, sb)
      sb.append(" ").append(op.symbol).append(" ")
      walk(right, depth + 1, indented, sb)

  /** Parentheses around a lone variable or constant are not printed (matches `(x)` & `(((x)))`). */
  private def isAtomicForStrip(e: Expr): Boolean =
    e match
      case X | _: Const  => true
      case Group(inner)  => isAtomicForStrip(inner)
      case _             => false

  private def constString(v: Double): String =
    val t = v.toString
    if t.contains('.') || t.contains('E') || t.contains('e') then t
    else t + ".0"

}
