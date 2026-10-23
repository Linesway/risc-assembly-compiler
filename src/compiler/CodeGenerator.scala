package standalone.compiler

import standalone.*
import ProgramRepresentation.*
import CodeBuilders.*
import Typer.*
import standalone.compiler.Assembler.{LIS, encodeUnsigned}
import standalone.runtime.Word
import standalone.syntax.Grammars.*


object CodeGenerator {
  def generateProcedures(typedProcedures: TypedProcedures) = {


    def generateCode(procedureScope: ProcedureScope): Unit = {

      val symbolTable = typedProcedures.symbolTables(procedureScope)



      def recur(node: Tree): Code = {

        val kids = node.children
        val nodeKind = node.lhs.kind

        def handleBinOp(left: Tree, opKind: String, right: Tree): Code =
          val op = opKind match
            case "PLUS" => plus
            case "MINUS" => minus
            case "STAR" => times
            case "SLASH" => divide
            case "PCT" => remainder
          binOp(recur(left), op, recur(right))

        nodeKind match {

          case "ID" =>
            symbolTable(node.lhs.lexeme) match
              case Left(tv) => read(Reg.result, tv.variable)
              case Right(ps) => Closure(ps.procedure)

          case "factor" =>
            kids.head.lhs.kind match
              case "ID" => recur(kids.head)
              case "NUM" =>
                val value = kids.head.lhs.lexeme.toLong
                block(LIS(Reg.result), Word(encodeUnsigned(value)))
              case "LPAREN" => recur(kids(1))
              case "factor" =>
                val functionCode = recur(kids.head)
                val argExprs = collect(kids(2), "expr").map(recur)
                functionCode match
                  case Closure(proc) if kids.head.children.head.lhs.kind == "ID" => Call(proc, argExprs)
                  case closure =>
                    val FunctionType(paramTypes, _) = typedProcedures.typeMap(kids.head): @unchecked
                    val params = paramTypes.map {
                      case IntType => Variable("int")
                      case FunctionType(_, _) => Variable("closure", isPointer = true)
                    }
                    CallClosure(closure, argExprs, params)

          case "term" =>
            kids.head.lhs.kind match
              case "factor" => recur(kids.head)
              case "term" => handleBinOp(kids.head, kids(1).lhs.kind, kids(2))

          case "expr" =>
            kids.head.lhs.kind match
              case "term" => recur(kids.head)
              case "expr" => handleBinOp(kids.head, kids(1).lhs.kind, kids(2))
              case "IF" =>
                val Seq(_, _, test, _, _, thenBranch, _, _, _, elseBranch, _) = kids
                val testOp = test.children(1).lhs.kind match
                  case "NE" => neCmp
                  case "LT" => ltCmp
                  case "LE" => leCmp
                  case "GE" => geCmp
                  case "GT" => gtCmp
                  case "EQ" => eqCmp
                ifStmt(
                  recur(test.children.head),
                  testOp,
                  recur(test.children(2)),
                  recur(thenBranch),
                  recur(elseBranch)
                )

          case "expra" =>
            kids.head.lhs.kind match
              case "ID" =>
                val Left(TypedVariable(variable, _)) = symbolTable(kids.head.lhs.lexeme): @unchecked
                assign(variable, recur(kids(2)))
              case "expr" => recur(kids.head)

          case "expras" =>
            if kids.tail.isEmpty then recur(kids.head)
            else block(recur(kids.head), recur(kids(2)))

          case _ =>
            // fallback for unknown nodes
            Block(kids.map(recur))
        }
      }



      procedureScope.procedure.code = Scope(procedureScope.variables.map(_.variable), recur(procedureScope.expras))
    }



    typedProcedures.procedureScopes.foreach(generateCode)
    typedProcedures.procedureScopes.map(_.procedure)
  }
}
