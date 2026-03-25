/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

/**
  * Parse failure with a caret offset into the (trimmed) expression text.
  */
class ParseError(val offset: Int, message: String, val source: String)
    extends IllegalArgumentException(
      if offset >= 0 then s"$message (at offset $offset in '$source')"
      else s"$message (in '$source')"
    )
