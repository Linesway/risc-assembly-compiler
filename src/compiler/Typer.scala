package standalone.compiler

import ProgramRepresentation.*
import standalone.syntax.Grammars.*

import scala.collection.mutable


object Typer {

  sealed abstract class Type
  case object IntType extends Type
  case object DummyType extends Type
  case class FunctionType(parameterTypes: Seq[Type], returnType: Type) extends Type


  def collect(tree: Tree, lhsKind: String): Seq[Tree] =
    if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))


  def parseType(tree: Tree): Type = {
    val types = collect(tree, "type")
    require(types.size == 1)

    def parseTypes(tree: Tree): Seq[Type] = tree.children match {
      case Seq(typeNode) =>
        Seq(parseType(typeNode))
      case Seq(typeNode, comma, rest) =>
        parseType(typeNode) +: parseTypes(rest)
      case _ =>
        Seq()
    }

    types.head.children match {
      // type -> INT
      case Seq(token) if token.lhs.kind == "INT" =>
        IntType

      // type -> LPAREN typesopt RPAREN ARROW type
      case Seq(lp, typesopt, rp, arrow, ret) =>
        val paramTypes =
          if (typesopt.children.isEmpty) Seq.empty
          else parseTypes(typesopt.children.head)

        val returnType = parseType(ret)
        FunctionType(paramTypes, returnType)

      case _ =>
        sys.error("Invalid type node")
    }
  }

  case class TypedVariable(variable: Variable, tpe: Type)


  def makeVariable(name: String, tpe: Type): Variable =
    new Variable(name, isPointer = (tpe != IntType))


  type SymbolTable = Map[String, Either[TypedVariable, ProcedureScope]]


  def parseVarDefs(tree: Tree): Seq[TypedVariable] = {
    collect(tree, "vardef").map { varDef =>
      val idNode = varDef.children.find(_.lhs.kind == "ID")
        .getOrElse(sys.error("vardef missing ID"))
      TypedVariable(makeVariable(idNode.lhs.lexeme, parseType(varDef)), parseType(varDef))
    }
  }


  def checkDuplicates(names: Seq[String]): Unit = {
    val duplicates = names.diff(names.distinct)
    if(duplicates.nonEmpty) //sys.error(s"Duplicate identifiers ${duplicates}")
      sys.error(s"Duplicate identifiers found: ${duplicates.mkString(", ")} (count: ${duplicates.size})")
  }


  class ProcedureScope(tree: Tree, outer: Option[ProcedureScope] = None) {
    assert(tree.production ==
      "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE")
    val Seq(_, id, _, parmsopt, _, _, retTypeTree, _, _, vardefs, defdefs, expras, _) = tree.children


    val name: String = id.lhs.lexeme


    val parms: Seq[TypedVariable] = parseVarDefs(parmsopt)


    val variables: Seq[TypedVariable] = parseVarDefs(vardefs)


    val returnType: Type = parseType(retTypeTree)


    val tpe: FunctionType = FunctionType(parms.map(_.tpe), returnType)


    val procedure: Procedure = Procedure(name, parms.map(_.variable), outer.map(_.procedure))


    val subProcedures: Seq[ProcedureScope] = collect(defdefs, "defdef").map { tree =>
      new ProcedureScope(tree, Some(this))
    }


    val newNames: Seq[String] =
      parms.map(_.variable.name) ++
        variables.map(_.variable.name) ++
        subProcedures.map(_.name)
    checkDuplicates(newNames)


    def symbolTable(outer: SymbolTable): SymbolTable = {
      // build inner map from parameters and local variables
      val varEntries: Iterable[(String, Either[TypedVariable, ProcedureScope])] =
        (parms ++ variables).map(tv => tv.variable.name -> Left(tv))

      // build inner map from subprocedures
      val procEntries: Iterable[(String, Either[TypedVariable, ProcedureScope])] =
        subProcedures.map(ps => ps.name -> Right(ps))

      // merge outer map with inner entries
      outer ++ (varEntries ++ procEntries)
    }


    def descendantScopes: Seq[ProcedureScope] = this +: subProcedures.flatMap(_.descendantScopes)

    override def toString = s"ProcedureScope for $name"
  }


  def createSymbolTables(topLevelProcedureScopes: Seq[ProcedureScope], topLevelSymbolTable: SymbolTable):
  Map[ProcedureScope, SymbolTable] = {
    def recur(procedureScopes: Seq[ProcedureScope], outerSymbolTable: SymbolTable): Map[ProcedureScope, SymbolTable] = {
      procedureScopes.flatMap{ procedureScope =>
        val symbolTable = procedureScope.symbolTable(outerSymbolTable)
        Map(procedureScope -> symbolTable) ++ recur(procedureScope.subProcedures, symbolTable)
      }.toMap
    }
    recur(topLevelProcedureScopes, topLevelSymbolTable)
  }



  def typeCheck(scope: ProcedureScope, symbolTable: SymbolTable): Map[Tree, Type] = {

    val treeToType = mutable.Map[Tree, Type]()


    def mustEqual(tpe1: Type, tpe2: Type): Type =
      if(tpe1 == tpe2) tpe1 else sys.error(s"Type mismatch: expected $tpe2, got $tpe1")


    def typeOf(node: Tree): Type = {
      def computeType: Type = {
        val kids = node.children
        val kind = node.lhs.kind

        kind match {

          case "ID" =>
            symbolTable(node.lhs.lexeme) match
              case Left(tv)  => tv.tpe
              case Right(ps) => ps.tpe
              case _         => DummyType

          case "factor" =>
            val firstChildKind = kids.head.lhs.kind
            firstChildKind match
              case "ID"    => typeOf(kids.head)
              case "NUM"   => IntType
              case "LPAREN"=> typeOf(kids(1))
              case "factor" =>
                val funcType = typeOf(kids.head)
                funcType match
                  case IntType =>
                    sys.error("Cannot apply a non-function value")
                  case FunctionType(params, ret) =>
                    val argTypes = collect(kids(2), "expr").map(typeOf)
                    if argTypes != params then
                      sys.error(s"Argument types $argTypes do not match expected $params")
                    ret

          case "term" =>
            val childKind = kids.head.lhs.kind
            childKind match
              case "factor" => typeOf(kids.head)
              case "term" =>
                val lhsType = typeOf(kids.head)
                val rhsType = typeOf(kids(2))
                mustEqual(lhsType, IntType)
                mustEqual(rhsType, IntType)
                IntType

          case "expr" =>
            kids.head.lhs.kind match
              case "term" => typeOf(kids.head)
              case "expr" =>
                mustEqual(typeOf(kids.head), IntType)
                mustEqual(typeOf(kids(2)), IntType)
                IntType
              case "IF" =>
                val Seq(_, _, testNode, _, _, thenNode, _, _, _, elseNode, _) = kids
                mustEqual(typeOf(testNode.children.head), IntType)
                mustEqual(typeOf(testNode.children(2)), IntType)
                val thenType = typeOf(thenNode)
                val elseType = typeOf(elseNode)
                mustEqual(thenType, elseType)
                thenType

          case "expra" =>
            kids.head.lhs.kind match
              case "ID" =>
                symbolTable(kids.head.lhs.lexeme) match
                  case Right(_) => sys.error("Cannot assign to a procedure")
                  case _        => ()
                val leftType  = typeOf(kids.head)
                val rightType = typeOf(kids(2))
                mustEqual(leftType, rightType)
                leftType
              case "expr" =>
                typeOf(kids.head)

          case "expras" =>
            if kids.tail.isEmpty then typeOf(kids.head)
            else
              typeOf(kids.head)
              typeOf(kids(2))

          case _ =>
            DummyType
        }
      }

      treeToType.getOrElseUpdate(node, computeType)
    }
    // check the root type
    val rootType = typeOf(scope.expras)
    if (rootType == DummyType)
      sys.error("Program has no valid expressions (only dummy nodes)")


    mustEqual(scope.returnType, typeOf(scope.expras))

    Map() ++ treeToType
  }


  case class TypedProcedures(
                              procedureScopes: Seq[ProcedureScope],
                              symbolTables: ProcedureScope=>SymbolTable,
                              typeMap: PartialFunction[Tree,Type]) {


    def showTree(tree: Tree, indent: Int = 0): String = {
      val typeString = typeMap.lift.apply(tree) match {
        case Some(tpe) => ": " + tpe
        case None => ""
      }
      " " * indent + tree.lhs + typeString + "\n" +
        tree.children.map(ch => showTree(ch, indent+1)).mkString
    }

    override def toString = procedureScopes.map{ procedureScope =>
      procedureScope.toString + "\n" +
        "Symbol table:\n" +
        symbolTables(procedureScope).map{case (name, meaning) => s" $name -> $meaning\n"}.mkString +
        "Procedure body with types:\n" +
        showTree(procedureScope.expras)
    }.mkString("\n")
  }


  def typeTree(tree: Tree): TypedProcedures = {
    assert(tree.production == "S BOF defdefs EOF")
    val defdefs = tree.children(1)

    val topLevelProcedureScopes = collect(defdefs, "defdef").map{defdef => new ProcedureScope(defdef, None)}
    checkDuplicates(topLevelProcedureScopes.map(procedureScope => procedureScope.name))
    val topLevelSymbolTable: SymbolTable =
      topLevelProcedureScopes.map{procedure => (procedure.name -> Right(procedure))}.toMap
    val symbolTables = createSymbolTables(topLevelProcedureScopes, topLevelSymbolTable)

    val allProcedureScopes = topLevelProcedureScopes.flatMap(procedureScope => procedureScope.descendantScopes)

    val typeMap: Map[Tree, Type] = allProcedureScopes.flatMap(procedureScope =>
      typeCheck(procedureScope, symbolTables(procedureScope))).toMap

    val mainProcedure = topLevelProcedureScopes.head
    if(mainProcedure.tpe != FunctionType(Seq(IntType, IntType), IntType))
      sys.error("The type of the main procedure must be (Int, Int)=>Int.")

    TypedProcedures(allProcedureScopes, symbolTables, typeMap)
  }
}
