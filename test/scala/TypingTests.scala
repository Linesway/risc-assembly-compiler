import standalone.compiler.*
import standalone.compiler.ProcedureExamples.*
import standalone.compiler.AutomataExamples.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.Lacs.*
import standalone.compiler.MemoryManagement.Chunk
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Scanning.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite
import standalone.compiler.Typer.*
import standalone.syntax.DFAs.*
import standalone.syntax.Grammars.*

import scala.collection.mutable


class TypingTests extends AnyFunSuite {

  def mkNode(kind: String, lexeme: String = "", children: Seq[Tree] = Seq.empty): Tree =
    Tree(Token(kind, lexeme), children)

  def printScope(scope: Typer.ProcedureScope): Unit = {
    println(s"Procedure name: ${scope.name}")
    println(s"Parameters: ${scope.parms.map(_.variable.name).mkString(", ")}")
    println(s"Variables: ${scope.variables.map(_.variable.name).mkString(", ")}")
    println(s"Subprocedures: ${scope.subProcedures.map(_.name).mkString(", ")}")
    println(s"Return type: ${scope.returnType}")
    println(s"All new names: ${scope.newNames.mkString(", ")}")
    println(s"Descendant scopes: ${scope.descendantScopes.map(_.name).mkString(", ")}")
    println("-" * 40)
  }

  test("Empty procedure") {
    val emptyTree = mkNode("defdef", children = Seq(
      mkNode("DEF"),
      mkNode("ID", "main"),
      mkNode("LPAREN"),
      mkNode("parmsopt"),
      mkNode("RPAREN"),
      mkNode("COLON"),
      mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt"),
      mkNode("defdefsopt"),
      mkNode("expras"),
      mkNode("RBRACE")
    ))

    val emptyScope = new Typer.ProcedureScope(emptyTree, None)
    printScope(emptyScope)
  }

  test("Procedure with parameters and variables") {
    val paramVarTree = mkNode("defdef", children = Seq(
      mkNode("DEF"),
      mkNode("ID", "foo"),
      mkNode("LPAREN"),
      mkNode("parmsopt", children = Seq(
        mkNode("vardef", children = Seq(mkNode("ID", "x"), mkNode("type", children = Seq(mkNode("INT"))))),
        mkNode("vardef", children = Seq(mkNode("ID", "y"), mkNode("type", children = Seq(mkNode("INT")))))
      )),
      mkNode("RPAREN"),
      mkNode("COLON"),
      mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt", children = Seq(
        mkNode("vardef", children = Seq(mkNode("ID", "z"), mkNode("type", children = Seq(mkNode("INT")))))
      )),
      mkNode("defdefsopt"),
      mkNode("expras"),
      mkNode("RBRACE")
    ))

    val paramVarScope = new Typer.ProcedureScope(paramVarTree, None)
    printScope(paramVarScope)
  }

  test("Procedure with nested subprocedure") {
    val innerTree = mkNode("defdef", children = Seq(
      mkNode("DEF"),
      mkNode("ID", "inner"),
      mkNode("LPAREN"),
      mkNode("parmsopt"),
      mkNode("RPAREN"),
      mkNode("COLON"),
      mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt"),
      mkNode("defdefsopt"),
      mkNode("expras"),
      mkNode("RBRACE")
    ))

    val outerTree = mkNode("defdef", children = Seq(
      mkNode("DEF"),
      mkNode("ID", "outer"),
      mkNode("LPAREN"),
      mkNode("parmsopt"),
      mkNode("RPAREN"),
      mkNode("COLON"),
      mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt"),
      mkNode("defdefsopt", children = Seq(innerTree)),
      mkNode("expras"),
      mkNode("RBRACE")
    ))

    val outerScope = new Typer.ProcedureScope(outerTree, None)
    printScope(outerScope)
    printScope(outerScope.subProcedures.head)
  }

  test("Procedure with duplicate parameter names") {
    val dupTree = mkNode("defdef", children = Seq(
      mkNode("DEF"),
      mkNode("ID", "bar"),
      mkNode("LPAREN"),
      mkNode("parmsopt", children = Seq(
        mkNode("vardef", children = Seq(mkNode("ID", "a"), mkNode("type", children = Seq(mkNode("INT"))))),
        mkNode("vardef", children = Seq(mkNode("ID", "a"), mkNode("type", children = Seq(mkNode("INT")))))
      )),
      mkNode("RPAREN"),
      mkNode("COLON"),
      mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt"),
      mkNode("defdefsopt"),
      mkNode("expras"),
      mkNode("RBRACE")
    ))

    try {
      new Typer.ProcedureScope(dupTree, None)
    } catch {
      case e: RuntimeException => println(s"Caught expected exception: ${e.getMessage}")
    }
  }
}

class TypeOfHardTests extends AnyFunSuite {

  def mkNode(kind: String, lexeme: String = "", children: Seq[Tree] = Seq.empty): Tree =
    Tree(Token(kind, lexeme), children)


  def printTreeTypes(treeToType: Map[Tree, Type]): Unit = {
    treeToType.foreach { case (tree, tpe) =>
      println(s"${tree.lhs.kind} (${tree.lhs.lexeme}) -> $tpe")
    }
  }

  test("assigning function to int variable should throw") {
    val inner = mkNode("defdef", children = Seq(
      mkNode("DEF"), mkNode("ID", "f"),
      mkNode("LPAREN"), mkNode("parmsopt"),
      mkNode("RPAREN"),
      mkNode("COLON"),
      mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt"), mkNode("defdefsopt"),
      mkNode("expras", children = Seq(mkNode("NUM", "1"))),
      mkNode("RBRACE")
    ))

    val main = mkNode("defdef", children = Seq(
      mkNode("DEF"), mkNode("ID", "main"),
      mkNode("LPAREN"), mkNode("parmsopt"), mkNode("RPAREN"),
      mkNode("COLON"), mkNode("type", children = Seq(mkNode("INT"))),
      mkNode("BECOMES"),
      mkNode("LBRACE"),
      mkNode("vardefsopt", children = Seq(
        mkNode("VAR"),
        mkNode("vardef", children = Seq(
          mkNode("ID", "x"),
          mkNode("COLON"),
          mkNode("type", children = Seq(mkNode("INT")))
        )),
        mkNode("SEMI")
      )),
      mkNode("defdefsopt", children = Seq(inner)),
      mkNode("expras", children = Seq(
        mkNode("expra", children = Seq(
          mkNode("ID", "x"),
          mkNode("BECOMES"),
          mkNode("ID", "f") // type mismatch
        ))
      )),
      mkNode("RBRACE")
    ))

    val scope = new ProcedureScope(main, None)
    val error = intercept[RuntimeException] {
      Typer.typeCheck(scope, scope.symbolTable(Map.empty))
    }
    assert(error.getMessage.contains("Type mismatch"))
  }

}
