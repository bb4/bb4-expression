package com.barrybecker4.expression

import com.barrybecker4.expression.mathexpression.MathExpressionParser

object ExpressionDemo:

  def main(args: Array[String]): Unit =
    println("Enter an expression (involving x) to parse.")
    val inputExp = scala.io.StdIn.readLine()
    val parser = new MathExpressionParser()
    val tree = parser.parse(inputExp)
    println("\nThe parsed expression is:")
    println(new TreeSerializer().regularSerialize(tree))
