/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Package-level aliases for [[mathexpression.MathOperators]] cases so existing
  * `import com.barrybecker4.expression.mathexpression.PLUS` (etc.) keep working
  * after the case-object → enum conversion.
  *
  * Note: this is source-compatible, not binary-compatible with the old `PLUS$`
  * case-object class files.
  */
package object mathexpression:
  val PLUS: MathOperators = MathOperators.PLUS
  val MINUS: MathOperators = MathOperators.MINUS
  val TIMES: MathOperators = MathOperators.TIMES
  val DIVIDE: MathOperators = MathOperators.DIVIDE
  val EXPONENT: MathOperators = MathOperators.EXPONENT
