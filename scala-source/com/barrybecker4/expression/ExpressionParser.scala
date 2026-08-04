/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

import scala.collection.mutable.ListBuffer

/**
  * Parses the text form of an expression into a tree representation.
  * See derived classes for specific sorts of expression parsers.
  * Called recursively to parse sub-expressions nested within parenthesis.
  */
abstract class ExpressionParser(val opDef: OperatorsDefinition) {

  /** @param textExpression the expression to parse. Must not be null or empty.
    * @return the root [[Expr]] of the parsed expression tree.
    */
  def parse(textExpression: String): Expr = {
    if (textExpression == null)
      throw new IllegalArgumentException("Expression must not be null")
    val exp = textExpression.trim
    if (exp.isEmpty)
      throw new IllegalArgumentException("Expression must not be empty")
    val nodes = getNodesAtLevel(exp)
    makeTreeFromNodes(nodes.toSeq)
  }

  /** Recursive method to find all cells for the terms at the current parenthesized level. */
  protected def getNodesAtLevel(exp: String): ListBuffer[ParseListItem]

  /** @param exp the whole sub expression
    * @param pos location of left parenthesis (the `(` in the source)
    * @return location of matching right parenthesis
    */
  protected def findClosingParen(exp: String, pos: Int): Int = {
    var parenCount = 1
    var i = pos
    var ch = '\u0000'
    while (!(ch == LexToken.RightParen.symbol && parenCount == 0) && i < exp.length) {
      ch = exp.charAt(i)
      i += 1
      if (ch == LexToken.LeftParen.symbol) parenCount += 1
      if (ch == LexToken.RightParen.symbol) parenCount -= 1
    }

    if (ch != LexToken.RightParen.symbol && i == exp.length)
      throw new ParseError(pos, "Mismatched parenthesis", exp)
    i - 1
  }

  /** Parse a parenthesized sub expression recursively.
    * @return current token. May have been reset to "".
    */
  protected def processSubExpression(exp: String, pos: Int, token: String,
                                     closingParenPos: Int, nodes: ListBuffer[ParseListItem]): String

  /** The token may represent several cells because of implicit multiplication. */
  protected def pushNodesForToken(token: String, nodes: ListBuffer[ParseListItem], exp: String, tokenEndPos: Int): Unit

  /** Converts a flat list to a single [[Expr]] by reducing in order of operator precedence. */
  protected def makeTreeFromNodes(nodes: Seq[ParseListItem]): Expr

  protected def splice(nodes: ListBuffer[ParseListItem], start: Int, num: Int, newCell: ParseListItem): Unit = {
    nodes.remove(start, num)
    nodes.insert(start, newCell)
  }

  protected def isOperatorCell(item: ParseListItem, ops: Array[Operator]): Boolean =
    item match
      case PlOperator(o) => ops.contains(o)
      case _             => false
}
