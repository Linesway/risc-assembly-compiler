package standalone.syntax

import DFAs.Token

object Grammars {
  case class Production(lhs: String, rhs: Seq[String]) {
    override def toString: String = (lhs +: rhs).mkString(" ")
  }
  case class Grammar(productions: Seq[Production]) {
    require(productions.nonEmpty)
    val nonTerminals: Set[String] = productions.map(_.lhs).toSet
    val symbols: Set[String] = nonTerminals ++ productions.flatMap(_.rhs)
    val terminals: Set[String] = symbols -- nonTerminals
    val start: String = productions.head.lhs
    val productionsExpanding: Map[String, Seq[Production]] =
      productions.groupBy(_.lhs).withDefaultValue(Seq.empty)
  }
  class Tree(val lhs: Token, val children: Seq[Tree] = Seq.empty) {
    def production: String = (lhs.kind +: children.map(_.lhs.kind)).mkString(" ")
    def show(indent: Int = 0): String =
      " " * indent + lhs + "\n" + children.map(_.show(indent + 1)).mkString
    override def toString: String = show()
  }
  def parseGrammar(input: String): Grammar = Grammar(
    input.linesIterator.map(_.trim).filter(_.nonEmpty).map { line =>
      val symbols = line.split("\\s+").toSeq
      Production(symbols.head, symbols.tail)
    }.toSeq
  )
}
