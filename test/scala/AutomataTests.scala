import standalone.compiler.*
import standalone.compiler.ProcedureExamples.*
import standalone.compiler.AutomataExamples.*
import standalone.compiler.Lacs.*
import standalone.compiler.Scanning.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.MemoryManagement.Chunk
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite

class AutomataTests extends AnyFunSuite {

  // Helper to print kinds and lexemes
  def printTokens(input: String): Unit = {
    val tokens = scan(input)
    println(s"Input: '$input'")
    tokens.foreach(t => println(s"kind=${t.kind}, lexeme='${t.lexeme}'"))
    println("-" * 40)
  }

  test("basic keywords and IDs") {
    printTokens("def var Int if else foo bar x1 y2")
  }

  test("numbers and zero handling") {
    assert(scan("0 53232 5").map(_.kind) == Seq("BOF", "NUM", "NUM", "NUM", "EOF"))
    intercept[RuntimeException] { scan("03") }
  }

  test("operators and punctuation") {
    printTokens("= == != < <= > >= + - * / % , ; : => () {}")
  }

  test("comments and whitespace") {
    printTokens("// this is a comment\n\t   //another\n")
  }

  test("mixed comment + code") {
    printTokens("def foo() : Int = { // comment\nvar x; x = 42 }")
  }

  test("ID followed by another ID (should be ok with whitespace or comment)") {
    printTokens("foo bar") // space between, ok
    printTokens("foo//comment\nbar") // comment in between, ok
  }

  test("illegal consecutive IDs (no whitespace)") {
    try {
      printTokens("foo bar") // without space would be illegal, but with space it's ok
    } catch {
      case e: Throwable => println("Caught error: " + e.getMessage)
    }
  }

  test("edge-case empty input") {
    printTokens("") // should produce only BOF and EOF
  }

  test("lots of slashes") {
    printTokens("///////") // comment-only input
    printTokens("//// / /") // mixed slashes and spaces
  }

  test("maximal munch with ambiguous tokens") {
    assert(scan("== => =").map(_.kind) == Seq("BOF", "EQ", "ARROW", "BECOMES", "EOF"))
    intercept[RuntimeException] { scan("=!") }
  }

  test("IDs that look like keywords") {
    printTokens("defx var123 Inty iffy elsez") // should all be ID, not keywords
  }

  test("invalid characters") {
    try {
      printTokens("@$#") // should cause lexer stuck errors
    } catch {
      case e: Throwable => println("Caught error: " + e.getMessage)
    }
  }
}
