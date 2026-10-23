package standalone.compiler

import scala.annotation.tailrec
import standalone.syntax.DFAs.*


object Scanning {

  def recognize(dfa: DFA, input: List[Char]): Boolean = {
    var result = true
    var state = dfa.start
    for(c <- input)
    {
      dfa.transition.lift((state, c)) match {
        case Some(next) =>
          state = next
        case None =>
          result = false
      }
    }
    if !result then result else dfa.accepting.contains(state)
  }



  def maximalMunchScan(dfa: DFA, input: String): Seq[Token] = {




    def scanOne(input: List[Char], state: State, backtrack: (List[Char], State)): (List[Char], State) = {
      val newBacktrack =
        if (dfa.accepting.contains(state)) (input, state) else backtrack

      if (input.isEmpty) newBacktrack
      else {
        dfa.transition.lift((state, input.head)) match {
          case Some(next) =>
            scanOne(input.tail, next, newBacktrack)
          case None =>
            newBacktrack
        }
      }
    }


    def listDiff[A](list: List[A], rest: List[A]): List[A] =
      if (list eq rest) Nil
      else list.head :: listDiff(list.tail, rest)



    def recur(input: List[Char], accumulator: List[Token] = List.empty): List[Token] = {
      if(input.isEmpty) then accumulator
      else {
        // Start scanning from the DFA's start state with backtrack = (input, state)
        val (tail, finalState) = scanOne(input, dfa.start, (input, dfa.start))

        // If scanOne didn't make progress, raise an error
        if (tail eq input) {
          sys.error(s"Lexer stuck: cannot scan token starting with '${input.head}'")
        } else {
          // Extract the lexeme scanned
          val lexemeChars = listDiff(input, tail)
          val lexeme = lexemeChars.mkString

          // Create a new token: (final DFA state, scanned string)
          val token = Token(finalState, lexeme)

          // Recur on the remaining input
          recur(tail, token :: accumulator)
        }
      }
    }

    recur(input.toList).reverse
  }
}
