package standalone.compiler

import standalone.syntax.*
import Grammars.*
import DFAs.*

import scala.collection.mutable


object Parsing {

  def parseCYK(grammar: Grammar, input: IndexedSeq[Token]): Option[Tree] = {

    val memo = mutable.Map[(Seq[String], Int, Int), Option[Seq[Tree]]]()


    def recur(lhs: List[String], from: Int, length: Int): Option[Seq[Tree]] = {

      // Check memo table first
      val key = (lhs, from, length)
      if (memo.contains(key)) then
        memo(key)

      else {
        // Add placeholder to break potential cycles
        memo(key) = None

        val headOption = lhs.headOption
        val tail = lhs.drop(1)

        // Case 1: lhs is empty → must match empty input
        if (lhs.isEmpty) {
          val r = if (length == 0) Some(Seq()) else None
          memo(key) = r
          r
        }

        // Case 2: lhs = a β (head is terminal)
        else if (headOption.exists(grammar.terminals.contains)) {
          val head = headOption.get
          if (length >= 1 && input(from).kind == head) { //x must match head
            recur(tail, from + 1, length - 1) match {
              case Some(trees) =>
                val res = Some(new Tree(input(from)) +: trees)
                memo(key) = res
                res
              case None =>
                memo(key) = None
                None
            }
          } else {
            memo(key) = None
            None
          }
        }

        // Case 3: lhs = A (single non-terminal)
        else if (lhs.size == 1 && grammar.nonTerminals.contains(headOption.get)) { // Case 3: single non-terminal
          val A = headOption.get
          val r = grammar.productions
            .filter(_.lhs == A)
            .iterator
            .map { prod =>
              recur(prod.rhs.toList, from, length).map(trees => Seq(new Tree(Token(A), trees)))
            }
            .collectFirst { case Some(trees) => trees }
          memo(key) = r
          r
        }

        // Case 4: lhs = A β (non-terminal + tail)
        else if (grammar.nonTerminals.contains(headOption.get)) {
          val A = headOption.get
          val r = (0 to length).iterator
            .map { split =>
              val x1Length = split
              val x2Length = length - split
              val trees1 = recur(List(A), from, x1Length)
              val trees2 = recur(tail, from + x1Length, x2Length)
              (trees1, trees2) match {
                case (Some(t1), Some(t2)) => Some(t1 ++ t2)
                case _ => None
              }
            }
            .collectFirst { case Some(trees) => trees }
          memo(key) = r
          r
        }

        // fallback, should not happen
        else {
          memo(key) = None
          None
        }
      }
    }

    recur(List(grammar.start), 0, input.size).map(_.head)
  }


  def parseEarley(grammar: Grammar, input: IndexedSeq[String]): Boolean = {

    case class State(lhs: String, rhs: List[String], dot: Int, start: Int)

    // chart(k) = set of states at position k in input
    val chart = Array.fill(input.length + 1)(mutable.Set[State]())

    // Add start rule: S' -> • S at chart(0)
    val startState = State(grammar.start, List(grammar.start), 0, 0)
    chart(0).add(startState)

    // Helper: add new state if not already present
    def addState(k: Int, s: State): Unit =
      if !chart(k).contains(s) then chart(k).add(s)

    for (k <- 0 to input.length) {

      var changed = true
      while (changed) {
        changed = false

        for (state <- chart(k).toList) {
          // Case 1: Predict
          if (state.dot < state.rhs.length && grammar.nonTerminals.contains(state.rhs(state.dot))) {
            val B = state.rhs(state.dot)
            for (prod <- grammar.productions if prod.lhs == B) {
              val newState = State(B, prod.rhs.toList, 0, k)
              if !chart(k).contains(newState) then
                chart(k).add(newState)
                changed = true
            }
          }

          // Case 2: Scan
          else if (state.dot < state.rhs.length && grammar.terminals.contains(state.rhs(state.dot))) {
            if (k < input.length && input(k) == state.rhs(state.dot)) {
              val newState = State(state.lhs, state.rhs, state.dot + 1, state.start)
              if !chart(k+1).contains(newState) then
                chart(k+1).add(newState)
            }
          }

          // Case 3: Complete
          else if (state.dot == state.rhs.length) {
            // Rule complete: A→α•
            for (prev <- chart(state.start)) {
              if (prev.dot < prev.rhs.length && prev.rhs(prev.dot) == state.lhs) {
                val newState = State(prev.lhs, prev.rhs, prev.dot + 1, prev.start)
                if !chart(k).contains(newState) then
                  chart(k).add(newState)
                  changed = true
              }
            }
          }
        }
      }
    }

    // Accept if completed start symbol covers whole input
    chart(input.length).exists(s =>
      s.lhs == grammar.start && s.dot == s.rhs.length && s.start == 0
    )
  }
}
