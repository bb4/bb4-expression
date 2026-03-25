// Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT
package com.barrybecker4.expression

import scala.util.Try

/**
  * Creates a tree from a text representation of an expression that is written in terms of x.
  * Use the companion `Expression.apply` factory so parse failures are recorded instead of printed.
  */
object Expression:

  /** Parse `expressionText` with `parser`; failures are available via `parseFailure`. */
  def apply(expressionText: String, parser: ExpressionParser): Expression =
    new Expression(expressionText, parser, Try(parser.parse(expressionText)))

class Expression private (
    val expressionText: String,
    val parser: ExpressionParser,
    private val parseResult: Try[Expr]
) {

  def isValid: Boolean = parseResult.isSuccess

  /** The root of the parse tree, if parsing succeeded. */
  def rootNode: Option[Expr] = parseResult.toOption

  /** @throws Exception the same exception that a direct parse would throw if parsing failed */
  def getRootNode: Expr = parseResult.get

  /** If parsing failed, the captured error (no stack trace is printed by this type). */
  def parseFailure: Option[Throwable] = parseResult.failed.toOption

}
