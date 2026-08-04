// Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT
package com.barrybecker4.expression.mathexpression

import com.barrybecker4.expression.Operator

/**
  * The expected binary operators in the text expression.
  * @author Barry Becker
  */
enum MathOperators(override val symbol: Char) extends Operator(symbol):
  case PLUS extends MathOperators('+')
  case MINUS extends MathOperators('-')
  case TIMES extends MathOperators('*')
  case DIVIDE extends MathOperators('/')
  case EXPONENT extends MathOperators('^')

  override def operate(operand1: Double, operand2: Double): Double = this match
    case PLUS     => operand1 + operand2
    case MINUS    => operand1 - operand2
    case TIMES    => operand1 * operand2
    case DIVIDE   => operand1 / operand2
    case EXPONENT => math.pow(operand1, operand2)

object MathOperators:

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
