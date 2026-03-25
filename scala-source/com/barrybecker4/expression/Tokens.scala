/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Lexical single-character tokens for scanning (distinct from math `Operator` symbols that share
  * the same character, e.g. minus as an operator vs minus as punctuation).
  */
enum LexToken(val symbol: Char):
  case Plus extends LexToken('+')
  case Minus extends LexToken('-')
  case LeftParen extends LexToken('(')
  case RightParen extends LexToken(')')
