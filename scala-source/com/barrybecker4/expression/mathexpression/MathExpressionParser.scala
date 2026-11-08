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
    while (pos < exp.length) {
      ch match {
        case c if Character.isWhitespace(c) =>
          // Whitespace ends the current token; it must not glue digits across a gap.
          if (token.nonEmpty) {
            pushNodesForToken(token, nodes, exp, math.max(0, pos - 1))
            token = ""
          }
        case c if c == LexToken.LeftParen.symbol =>
          val closingParenPos = findClosingParen(exp, pos + 1)
          token = processSubExpression(exp, pos + 1, token, closingParenPos, nodes)
          pos = closingParenPos
        case c if c == LexToken.Minus.symbol && token.isEmpty && opDef.isLastNodeOperator(nodes) =>
          token += c
        case c if isNumericChar(c) =>
          if (token.isEmpty && endsWithConst(nodes))
            throw new ParseError(pos, "Adjacent numeric literals are not allowed; missing operator?", exp)
          if (token.isEmpty && endsWithX(nodes))
            throw new ParseError(pos, "Cannot have numbers after x in a term", exp)
          appendImplicitTimesAfterParen(nodes, token)
          token += ch
          if (token.contains("x"))
            throw new ParseError(pos, "Cannot have numbers after x in a term " + token, exp)
        case c if (c == 'e' || c == 'E') && isNumberToken(token) =>
          val expEnd = scanExponent(exp, pos)
          token += exp.substring(pos, expEnd)
          pos = expEnd - 1
        case 'x' =>
          appendImplicitTimesAfterFactor(nodes, token)
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
    rejectDanglingOperators(nodes, exp, math.max(0, pos - 1))
    nodes
  }

  /** Catch leading/trailing operators while the character index is still known. */
  private def rejectDanglingOperators(nodes: ListBuffer[ParseListItem], exp: String, lastPos: Int): Unit = {
    if (nodes.isEmpty) return
    nodes.head match {
      case PlOperator(_) => throw new ParseError(0, "Missing operand", exp)
      case _             =>
    }
    nodes.last match {
      case PlOperator(_) => throw new ParseError(lastPos, "Missing operand", exp)
      case _             =>
    }
  }

  override protected def processSubExpression(exp: String, pos: Int,
                                              token: String, closingParenPos: Int,
                                              nodes: ListBuffer[ParseListItem]): String = {
    if (closingParenPos <= pos)
      throw new ParseError(pos, "Empty parenthesized subexpression", exp)
    val subExp = exp.substring(pos, closingParenPos)
    val subRoot = parse(subExp)
    val wrapped = Group(subRoot)
    // Unary minus before `(...)` is one operand: -1 * (group). Do not leave "-" as a token.
    if (token == String.valueOf(LexToken.Minus.symbol)) {
      nodes.append(PlOperand(BinOp(TIMES, Const(-1.0), wrapped)))
      return ""
    }
    var resultToken = token
    if (token.nonEmpty) {
      pushNodesForToken(token, nodes, exp, pos - 1)
      resultToken = ""
      nodes.append(PlOperator(TIMES))
    }
    else if (nodes.nonEmpty && endsWithOperand(nodes))
      nodes.append(PlOperator(TIMES))
    nodes.append(PlOperand(wrapped))
    resultToken
  }

  /**
    * Emit cells for a scanned term. Unary-minus terms (`-x`, `-2x`) are one pre-grouped
    * [[PlOperand]] so a following `^` or `/` cannot steal the minus. A positive coefficient
    * like `2x` stays as separate `Const`, `TIMES`, `X` cells so `^` still binds tighter
    * (`2x^2` → `2*(x^2)`, not `(2x)^2`).
    */
  override protected def pushNodesForToken(token: String, nodes: ListBuffer[ParseListItem], exp: String, errPos: Int): Unit = {
    if (token.isEmpty) return
    val len = token.length
    if (token.charAt(len - 1) == 'x') {
      if (len == 1)
        nodes.append(PlOperand(X))
      else {
        val coeffText = token.substring(0, len - 1)
        if (coeffText.charAt(0) == LexToken.Minus.symbol) {
          val coeff =
            if (coeffText == String.valueOf(LexToken.Minus.symbol)) Const(-1.0)
            else numberExpr(coeffText, exp, errPos)
          nodes.append(PlOperand(BinOp(TIMES, coeff, X)))
        } else {
          nodes.append(PlOperand(numberExpr(coeffText, exp, errPos)))
          nodes.append(PlOperator(TIMES))
          nodes.append(PlOperand(X))
        }
      }
    }
    else nodes.append(PlOperand(numberExpr(token, exp, errPos)))
  }

  /** Implicit `*` after a closed `(...)` when the next token starts a number (e.g. `(x+1)2`). */
  private def appendImplicitTimesAfterParen(nodes: ListBuffer[ParseListItem], token: String): Unit =
    if (token.isEmpty && nodes.nonEmpty && endsWithGroup(nodes))
      nodes.append(PlOperator(TIMES))

  /**
    * Implicit `*` when `x` follows a completed factor separated by whitespace
    * (e.g. `2 x`, `x x`, `(x+1) x`), or immediately after a closed group (`(x+1)x`).
    */
  private def appendImplicitTimesAfterFactor(nodes: ListBuffer[ParseListItem], token: String): Unit =
    if (token.isEmpty && nodes.nonEmpty && endsWithOperand(nodes))
      nodes.append(PlOperator(TIMES))

  private def endsWithGroup(nodes: ListBuffer[ParseListItem]): Boolean =
    nodes.nonEmpty && (nodes.last match
      case PlOperand(Group(_)) => true
      case _                   => false)

  private def endsWithConst(nodes: ListBuffer[ParseListItem]): Boolean =
    nodes.nonEmpty && (nodes.last match
      case PlOperand(Const(_)) => true
      case _                   => false)

  private def endsWithX(nodes: ListBuffer[ParseListItem]): Boolean =
    nodes.nonEmpty && (nodes.last match
      case PlOperand(X) => true
      case _            => false)

  private def endsWithOperand(nodes: ListBuffer[ParseListItem]): Boolean =
    nodes.nonEmpty && (nodes.last match
      case PlOperand(_) => true
      case _            => false)

  private def isNumericChar(ch: Char) = (ch >= '0' && ch <= '9') || ch == '.'

  /** True when `token` is a numeric literal in progress (optional leading minus, digits/dot only). */
  private def isNumberToken(token: String): Boolean =
    token.nonEmpty && !token.contains('x') && token.exists(ch => ch >= '0' && ch <= '9')

  /**
    * Consume an optional exponent `[eE][+-]?digits` starting at `pos`.
    * Requires at least one digit after the optional sign; otherwise the `e`/`E` is unexpected.
    */
  private def scanExponent(exp: String, pos: Int): Int = {
    var i = pos + 1
    if (i < exp.length && (exp.charAt(i) == '+' || exp.charAt(i) == '-')) i += 1
    val digitStart = i
    while (i < exp.length && exp.charAt(i) >= '0' && exp.charAt(i) <= '9') i += 1
    if (i == digitStart)
      throw new ParseError(pos, "Unexpected character '" + exp.charAt(pos) + "' in expression", exp)
    i
  }

  private def numberExpr(token: String, exp: String, errPos: Int): Expr =
    try {
      val num = token.toDouble
      if (num.isNaN || num.isInfinite)
        throw new ParseError(errPos, "Non-finite (infinite or NaN) number in expression: " + token, exp)
      Const(num)
    } catch {
      case _: NumberFormatException =>
        throw new ParseError(errPos, "Invalid number in expression: " + token, exp)
    }
}
