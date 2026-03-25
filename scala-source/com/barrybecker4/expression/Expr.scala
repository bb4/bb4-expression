/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Parsed expression as an immutable algebraic data type.
  * Binary operations are left-associative at a given precedence when built by the parser reducer
  * (except `^`, which is also reduced left-to-right; see [[com.barrybecker4.expression.mathexpression.MathOperatorsDefinition]]).
  *
  * Use [[Group]] for subexpressions that were explicitly parenthesized in the source; the serializer
  * may omit redundant parentheses around atoms ([[X]], [[Const]]).
  */
sealed trait Expr

/** The only variable supported by [[com.barrybecker4.expression.mathexpression.MathExpressionParser]]. */
case object X extends Expr

case class Const(value: Double) extends Expr

case class BinOp(op: Operator, left: Expr, right: Expr) extends Expr

/** Subexpression that appeared inside `( ... )` in the source. */
case class Group(inner: Expr) extends Expr

/** Token in the flat list before precedence reduction. */
sealed trait ParseListItem

case class PlOperand(expr: Expr) extends ParseListItem

case class PlOperator(op: Operator) extends ParseListItem
