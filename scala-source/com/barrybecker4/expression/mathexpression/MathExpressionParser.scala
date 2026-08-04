/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression.mathexpression

import com.barrybecker4.expression.*

import scala.collection.mutable.ListBuffer

/**
  * Parses the text form of an expression (in `x`) into an [[Expr]] tree.
  */
class MathExpressionParser(opDef: OperatorsDefinition = new MathOperatorsDefinition()) extends ExpressionParser(opDef) {

  override protected def getNodesAtLevel(exp: String): ListBuffer[ParseListItem] =
    scanLevel(exp)

  private def scanLevel(exp: String): ListBuffer[ParseListItem] = {
    val nodes = ListBuffer[ParseListItem]()
    var pos = 0
    var token = ""
    var ch = exp.charAt(pos)
    val closingToken = "" + LexToken.RightParen.symbol
    while (pos < exp.length && token != closingToken) {
      ch match {
        case ' ' =>
        case c if c == LexToken.LeftParen.symbol =>
          val closingParenPos = findClosingParen(exp, pos + 1)
          token = processSubExpression(exp, pos + 1, token, closingParenPos, nodes)
          pos = closingParenPos
        case c if c == LexToken.Minus.symbol && token.isEmpty && opDef.isLastNodeOperator(nodes) =>
          token += c
        case c if isNumericChar(c) =>
          appendImplicitTimesAfterParen(nodes, token)
          token += ch
          if (token.contains("x"))
            throw new ParseError(pos, "Cannot have numbers after x in a term " + token, exp)
        case 'x' =>
          appendImplicitTimesAfterParen(nodes, token)
          token += ch
        case c if opDef.isOperator(c) =>
          pushNodesForToken(token, nodes, exp, pos)
          token = ""
          nodes.append(PlOperator(MathOperators.operatorFor(c)))
        case c =>
          throw new ParseError(pos, "Unexpected character '" + c + "' in expression", exp)
      }
      pos += 1
      if (pos < exp.length) ch = exp.charAt(pos)
    }
    pushNodesForToken(token, nodes, exp, math.max(0, pos - 1))
    nodes
  }

  override protected def processSubExpression(exp: String, pos: Int,
                                              token: String, closingParenPos: Int,
                                              nodes: ListBuffer[ParseListItem]): String = {
    if (closingParenPos <= pos)
      throw new ParseError(pos, "Empty parenthesized subexpression", exp)
    val subExp = exp.substring(pos, closingParenPos)
    val subRoot = parse(subExp)
    val wrapped = Group(subRoot)
    var resultToken = token
    if (token.nonEmpty) {
      pushNodesForToken(token, nodes, exp, pos - 1)
      resultToken = ""
      nodes.append(PlOperator(TIMES))
    }
    else if (nodes.nonEmpty && endsWithGroup(nodes))
      nodes.append(PlOperator(TIMES))
    nodes.append(PlOperand(wrapped))
    resultToken
  }

  override protected def pushNodesForToken(token: String, nodes: ListBuffer[ParseListItem], exp: String, errPos: Int): Unit = {
    if (token.isEmpty) return
    val len = token.length
    if (token.charAt(len - 1) == 'x') {
      if (len > 1) {
        if (token.charAt(0) == LexToken.Minus.symbol) nodes.append(PlOperand(Const(-1)))
        else nodes.append(PlOperand(numberExpr(token.substring(0, len - 1), exp, errPos)))
        nodes.append(PlOperator(TIMES))
      }
      nodes.append(PlOperand(X))
    }
    else nodes.append(PlOperand(numberExpr(token, exp, errPos)))
  }

  private def appendImplicitTimesAfterParen(nodes: ListBuffer[ParseListItem], token: String): Unit =
    if (token.isEmpty && nodes.nonEmpty && endsWithGroup(nodes))
      nodes.append(PlOperator(TIMES))

  private def endsWithGroup(nodes: ListBuffer[ParseListItem]): Boolean =
    nodes.last match
      case PlOperand(Group(_)) => true
      case _                   => false

  private def isNumericChar(ch: Char) = (ch >= '0' && ch <= '9') || ch == '.'

  private def numberExpr(token: String, exp: String, errPos: Int): Expr =
    try {
      val num = token.toDouble
      if (num.isNaN) throw new ParseError(errPos, "Invalid number in expression: " + token, exp)
      Const(num)
    } catch {
      case _: NumberFormatException =>
        throw new ParseError(errPos, "Invalid number in expression: " + token, exp)
    }
}
