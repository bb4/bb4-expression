/* Copyright by Barry G. Becker, 2000-2018. Licensed under MIT License: http://www.opensource.org/licenses/MIT */
package com.barrybecker4.expression

import scala.collection.mutable.ListBuffer

/**
  * Reduces a flat infix token list to a single [[Expr]] by applying operator
  * precedence groups left-to-right within each level.
  */
private[expression] final class PrecedenceReducer(precedence: Array[Array[Operator]]) {

  def reduce(items: Seq[ParseListItem]): Expr = {
    if (items.isEmpty)
      throw new ParseError(-1, "Empty expression fragment", "")
    val nodes = ListBuffer.from(items)
    for (ops <- precedence) reduceNodes(ops, nodes)
    if (nodes.size > 1)
      throw new ParseError(-1, "Expected to have only one node after reducing, but have " + nodes.size + " : " + nodes, "")
    nodes.head match {
      case PlOperand(e)  => e
      case PlOperator(_) => throw new ParseError(-1, "Missing operands", "")
    }
  }

  private def reduceNodes(ops: Array[Operator], nodes: ListBuffer[ParseListItem]): Unit = {
    var index = 1
    if (nodes.size == 2)
      throw new ParseError(-1, "Missing operand : " + nodes, "")
    while (index < nodes.size) {
      if (isOperatorCell(nodes(index), ops)) {
        if (index + 1 >= nodes.size)
          throw new ParseError(-1, "Not enough operands for operator in nodes=" + nodes, "")
        (nodes(index - 1), nodes(index), nodes(index + 1)) match {
          case (PlOperand(l), PlOperator(op), PlOperand(r)) =>
            splice(nodes, index - 1, 3, PlOperand(BinOp(op, l, r)))
          case _ =>
            throw new ParseError(-1, "Malformed operator/operand sequence: " + nodes, "")
        }
      }
      else index += 2
    }
  }

  private def isOperatorCell(item: ParseListItem, ops: Array[Operator]): Boolean =
    item match
      case PlOperator(o) => ops.contains(o)
      case _             => false

  private def splice(nodes: ListBuffer[ParseListItem], start: Int, num: Int, newCell: ParseListItem): Unit = {
    nodes.remove(start, num)
    nodes.insert(start, newCell)
  }
}
