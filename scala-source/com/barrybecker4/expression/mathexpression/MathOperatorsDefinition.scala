/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT  */
package com.barrybecker4.expression.mathexpression

import com.barrybecker4.expression.{Operator, OperatorsDefinition, ParseListItem, PlOperand, PlOperator}

object MathOperatorsDefinition {

  /**
    * Operator precedence groups; within each group reduction is left-to-right.
    *
    * '''Note on `^`''' — many mathematical notations treat exponentiation as right-associative
    * (`a^b^c` as `a^(b^c)`). This library evaluates the same string as `((a^b)^c)` (left-associative
    * reduction), which matches the historical behavior tested in `MathExpressionParserExtendedSuite`.
    */
  private val OPERATOR_PRECEDENCE = Array(
    Array[Operator](EXPONENT),
    Array[Operator](TIMES, DIVIDE),
    Array[Operator](PLUS, MINUS)
  )

  private val SYMBOLS: Set[Char] = Set(PLUS, MINUS, TIMES, DIVIDE, EXPONENT).map(_.symbol)
}

/**
  * Binary operators and precedence for [[MathExpressionParser]].
  */
class MathOperatorsDefinition extends OperatorsDefinition {

  override def getOperatorPrecedence: Array[Array[Operator]] = MathOperatorsDefinition.OPERATOR_PRECEDENCE

  override def isOperator(ch: Char): Boolean = MathOperatorsDefinition.SYMBOLS.contains(ch)

  override def isLastNodeOperator(nodes: scala.collection.Seq[ParseListItem]): Boolean =
    nodes.isEmpty ||
      (nodes.last match
        case PlOperator(_) => true
        case PlOperand(_)  => false)
}
