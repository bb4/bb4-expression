package com.barrybecker4.expression

import java.util.Scanner

import com.barrybecker4.expression.mathexpression.{MathExpressionParser, MathOperatorsDefinition}


object ExpressionDemo extends App {

  val scanner = new Scanner(System.in)
  println("Enter an expression (involving x) to parse.")
  val inputExp = scanner.nextLine()

  val parser = new MathExpressionParser();
  val tree = parser.parse(inputExp)

  println("\nThe parsed expression is:")
  println(new TreeSerializer().regularSerialize(tree));
}
