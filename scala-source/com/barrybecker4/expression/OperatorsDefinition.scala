/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Operator symbols, precedence groups, and tokenizer hints for infix parsing.
  */
trait OperatorsDefinition {

  def isOperator(c: Char): Boolean

  /** Defines the order of precedence for the operators. Operators at the same level are left-associative. */
  def getOperatorPrecedence: Array[Array[Operator]]

  /**
    * @return true if the last emitted cell is an operator token, or there are no cells yet
    *         (used for unary minus after `+`, `-`, `*`, `/`, `^`, or at start).
    */
  /** Uses `scala.collection.Seq` so both `ListBuffer` and immutable sequences are accepted. */
  def isLastNodeOperator(nodes: scala.collection.Seq[ParseListItem]): Boolean
}
