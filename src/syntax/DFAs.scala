package standalone.syntax

object DFAs {
  type State = String
  case class DFA(
      alphabet: Set[Char], states: Set[State], start: State,
      accepting: Set[State], transition: PartialFunction[(State, Char), State]
  )
  case class Token(kind: String, lexeme: String = "") {
    override def toString: String = s"$kind $lexeme"
  }
}
