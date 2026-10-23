package standalone.compiler

import Scanning.*
import Parsing.*
import Typer.*
import CodeGenerator.*
import Transformations.*
import MemoryManagement.*
import standalone.syntax.*
import Grammars.*
import DFAs.*
import standalone.compiler.ProgramRepresentation.Procedure


object Lacs {


  val keywords = Set("def", "var", "Int", "if", "else")


  val symbols = Map(
    ' ' -> "WHITESPACE",
    '\t' -> "WHITESPACE",
    '\n' -> "WHITESPACE",
    '\r' -> "WHITESPACE",
    '0' -> "ZERO",
    '<' -> "LT",
    '>' -> "GT",
    '=' -> "BECOMES",
    '+' -> "PLUS",
    '-' -> "MINUS",
    '*' -> "STAR",
    '/' -> "SLASH",
    '%' -> "PCT",
    '(' -> "LPAREN",
    ')' -> "RPAREN",
    '{' -> "LBRACE",
    '}' -> "RBRACE",
    ',' -> "COMMA",
    ';' -> "SEMI",
    ':' -> "COLON"
  )



  val dfa = {

    DFA(
      alphabet = "<>=+-*/%(){},;:! \t\n\r".toSet ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'),
      states = Set("start",
        "ID",
        "zero", "NUM",
        "LPAREN",
        "RPAREN",
        "LBRACE",
        "RBRACE",
        "BECOMES",
        "EQ",
        "exclamation", "NE",
        "LT",
        "GT",
        "LE",
        "GE",
        "PLUS",
        "MINUS",
        "STAR",
        "SLASH",
        "PCT",
        "COMMA",
        "SEMI",
        "COLON",
        "ARROW",
        "COMMENT",
        "WHITESPACE"
      ),
      start = "start",
      accepting = Set(
        "ID",
        "zero", "NUM",
        "LPAREN",
        "RPAREN",
        "LBRACE",
        "RBRACE",
        "BECOMES",
        "EQ",
        "NE",
        "LT",
        "GT",
        "LE",
        "GE",
        "PLUS",
        "MINUS",
        "STAR",
        "SLASH",
        "PCT",
        "COMMA",
        "SEMI",
        "COLON",
        "ARROW",
        "COMMENT",
        "WHITESPACE"
      ),
      transition = {
        case ("start", ch) if ch.isLetter => "ID"
        case ("start", '0') => "zero"
        case ("start", d) if d.isDigit && d != '0' => "NUM"

        // single-character structural tokens
        case ("start", '(') => "LPAREN"
        case ("start", ')') => "RPAREN"
        case ("start", '{') => "LBRACE"
        case ("start", '}') => "RBRACE"
        case ("start", '+') => "PLUS"
        case ("start", '-') => "MINUS"
        case ("start", '*') => "STAR"
        case ("start", '/') => "SLASH" // may later transition to COMMENT
        case ("start", '%') => "PCT"
        case ("start", ',') => "COMMA"
        case ("start", ';') => "SEMI"
        case ("start", ':') => "COLON"
        case ("start", '=') => "BECOMES" // may later transition to EQ or ARROW
        case ("start", '!') => "exclamation" // may later transition to NE
        case ("start", '<') => "LT" // may later transition to LE
        case ("start", '>') => "GT" // may later transition to GE

        // whitespace
        case ("start", ' ') => "WHITESPACE"
        case ("start", '\t') => "WHITESPACE"
        case ("start", '\n') => "WHITESPACE"
        case ("start", '\r') => "WHITESPACE"

        case ("ID", ch) if ch.isLetterOrDigit => "ID"
        case ("NUM", d) if d.isDigit => "NUM"

        // '=' transitions
        case ("BECOMES", '=') => "EQ" // '=='
        case ("BECOMES", '>') => "ARROW" // '=>'

        // '!' transitions
        case ("exclamation", '=') => "NE" // '!='

        // '<' transitions
        case ("LT", '=') => "LE" // '<='

        // '>' transitions
        case ("GT", '=') => "GE" // '>='

        // '/' transitions: comment handling
        case ("SLASH", '/') => "COMMENT" // start of '//' comment
        case ("COMMENT", ch) if ch != '\n' => "COMMENT" // continue comment
      }
    )
  }


  def scan(input: String): Seq[Token] = {

    val nonConsecutiveIDs = Set("ID", "DEF", "VAR", "INT", "IF", "ELSE", "NUM")
    val nonConsecutiveTokens = Set("EQ", "NE", "LT", "LE", "GT", "GE", "BECOMES", "ARROW")

    def lacsScan(input: String): Seq[Token] = {
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

      def recur(input: List[Char], accumulator: List[Token] = List.empty, lastTokenKind: String = ""): List[Token] = {
        if (input.isEmpty) then accumulator
        else {
          // Start scanning from the DFA's start state with backtrack = (input, state)
          var (tail, finalState) = scanOne(input, dfa.start, (input, dfa.start))

          // If scanOne didn't make progress, raise an error
          if (tail eq input) {
            sys.error(s"Lexer stuck: cannot scan token starting with '${input.head}'")
          } else {
            // Extract the lexeme scanned
            val lexemeChars = listDiff(input, tail)
            val lexeme = lexemeChars.mkString

            if (finalState == "ID" && keywords.contains(lexeme)) {
              finalState = lexeme match {
                case "def" => "DEF"
                case "var" => "VAR"
                case "Int" => "INT"
                case "if" => "IF"
                case "else" => "ELSE"
                case _ => finalState
              }
            }
            if (finalState == "zero") {
              finalState = "NUM"
            }

            if(nonConsecutiveIDs.contains(finalState) && nonConsecutiveIDs.contains(lastTokenKind)) {
              sys.error(s"Lexer stuck: cannot have multiple consecutive IDs")
            }
            else if(nonConsecutiveTokens.contains(finalState) && nonConsecutiveTokens.contains(lastTokenKind)) {
              sys.error(s"Lexer stuck: forbidden consecutive tokens detected")
            }
            else {
              // Create a new token: (final DFA state, scanned string)
              val token = Token(finalState, lexeme)

              val newAccumulator = token.kind match {
                case "WHITESPACE" | "COMMENT" => accumulator // skip these
                case _ => token :: accumulator // keep all other tokens
              }

              // Recur on the remaining input
              recur(tail, newAccumulator, token.kind)
            }
          }
        }
      }
      recur(input.toList).reverse
    }
    val tokenBody = lacsScan(input)

    var result = Seq[Token]()
    result = result :+ Token("BOF")
    result = result ++ tokenBody
    result = result :+ Token("EOF")

    result
  }


  val grammar = parseGrammar("""
S BOF defdefs EOF
defdefs defdef defdefs
defdefs defdef
defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE
parmsopt parms
parmsopt
parms vardef COMMA parms
parms vardef
vardef ID COLON type
type INT
type LPAREN typesopt RPAREN ARROW type
typesopt types
typesopt
types type COMMA types
types type
vardefsopt VAR vardef SEMI vardefsopt
vardefsopt
defdefsopt defdefs
defdefsopt
expras expra SEMI expras
expras expra
expra ID BECOMES expr
expra expr
expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
expr term
expr expr PLUS term
expr expr MINUS term
term factor
term term STAR factor
term term SLASH factor
term term PCT factor
factor ID
factor NUM
factor LPAREN expr RPAREN
factor factor LPAREN argsopt RPAREN
test expr NE expr
test expr LT expr
test expr LE expr
test expr GE expr
test expr GT expr
test expr EQ expr
argsopt args
argsopt
args expr COMMA args
args expr
                             """
  )


  def scanAndParse(input: String): Tree = {
    val tokens = scan(input).toIndexedSeq
    val tree = parseCYK(grammar, tokens).getOrElse{
      val preview = tokens.take(20).map(_.lexeme).mkString(" ")
      sys.error("Parsing error near: " + preview)
    }
    tree
  }


  def scanAndParseAndType(input: String): TypedProcedures = {
    val tree = scanAndParse(input)
    typeTree(tree)
  }


  def scanAndParseAndTypeAndGenerate(input: String): Seq[Procedure] = {
    val typedProcedures = scanAndParseAndType(input)
    generateProcedures(typedProcedures)
  }


  def compile(input: String): MachineCode = {
    val procedures = scanAndParseAndTypeAndGenerate(input)
    compileClosures(procedures)
  }


  def compileWithGarbageCollector(input: String): MachineCode = {
    withGC {
      val procedures = scanAndParseAndTypeAndGenerate(input)
      compileClosures(procedures ++ GarbageCollector.procedures)
    }
  }
}
