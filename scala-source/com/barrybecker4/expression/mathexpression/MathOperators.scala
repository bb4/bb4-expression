// Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT
package com.barrybecker4.expression.mathexpression

import com.barrybecker4.expression.Operator

/**
  * The expected binary operators in the text expression.
  * @author Barry Becker
  */

case object PLUS extends Operator('+') {
  override def operate(operand1: Double, operand2: Double): Double = operand1 + operand2
}

case object MINUS extends Operator('-') {
  override def operate(operand1: Double, operand2: Double): Double = operand1 - operand2
}

case object TIMES extends Operator('*') {
  override def operate(operand1: Double, operand2: Double): Double = operand1 * operand2
}

case object DIVIDE extends Operator('/') {
  override def operate(operand1: Double, operand2: Double): Double = operand1 / operand2
}

case object EXPONENT extends Operator('^') {
  override def operate(operand1: Double, operand2: Double): Double = math.pow(operand1,  operand2)
}

object MathOperators {

  /** @throws IllegalArgumentException if `symbol` is not one of `+ - * / ^` */
  def operatorFor(symbol: Char): Operator =
    symbol match
      case '+' => PLUS
      case '-' => MINUS
      case '*' => TIMES
      case '/' => DIVIDE
      case '^' => EXPONENT
      case _ =>
        throw new IllegalArgumentException("Not an operator symbol: " + symbol)
}