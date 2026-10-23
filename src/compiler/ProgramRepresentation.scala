package standalone.compiler

import standalone.runtime
import standalone.runtime.*
import Debugger.DebugTable
import Assembler.*

import scala.collection.mutable


object ProgramRepresentation {



  class Label(val name: String) {
    override def toString = s"Label($name)"
  }


  sealed abstract class Code


  case class CodeWord(word: Word) extends Code


  implicit def toCodeWord(word: Word): Code = CodeWord(word)


  case class Define(label: Label) extends Code


  case class Use(label: Label) extends Code


  case class BeqBne(bits: Seq[Boolean], label: Label) extends Code


  def beq(s: Reg, t: Reg, label: Label): BeqBne = BeqBne(BEQ(s, t, 0).take(16), label)


  def bne(s: Reg, t: Reg, label: Label): BeqBne = BeqBne(BNE(s, t, 0).take(16), label)



  case class Comment(message: String) extends Code


  def createDebugTable(codes: Seq[Code]): DebugTable = {
    val ret = mutable.Map[Word,Seq[Comment|Label]]()
    var location = 0
    def locationWord = Word(encodeUnsigned(location * 4))
    def add(entry: Comment|Label): Unit = {
      val existing = ret.getOrElse(locationWord, Seq())
      ret(locationWord) = existing :+ entry
    }
    for(code <- codes) code match {
      case _:CodeWord | _:Use | _:BeqBne => location += 1
      case comment:Comment => add(comment)
      case Define(label) => add(label)
      case _ => require(false, s"Encountered unsupported code $code.")
    }
    Map() ++ ret
  }


  case class Block(stmts: Seq[Code]) extends Code


  def block(codes: Code*): Block = Block(codes)




  class Variable(val name: String, val isPointer: Boolean = false) {
    override def toString = s"Variable($name)"
  }


  case class VarAccess(register: Reg, variable: Variable, read: Boolean) extends Code {
    assert(read || register != Reg.scratch)
    override def toString =
      if(read) s"read $variable into $$$register"
      else s"write $$$register into $variable"
  }


  def read(register: Reg, variable: Variable): VarAccess = VarAccess(register, variable, read = true)


  def write(variable: Variable, register: Reg): VarAccess = VarAccess(register, variable, read = false)


  def assign(variable: Variable, expr: Code): Code = block(expr, write(variable, Reg.result))




  case class Scope(variables: Seq[Variable], code: Code) extends Code


  def ifStmt(e1: Code, comp: Label=>Code, e2: Code, thens: Code, elses: Code = Block(Seq())) = {
    val elseLabel = new Label("else")
    IfStmt(elseLabel, e1, comp(elseLabel), e2, thens, elses)
  }
  case class IfStmt(elseLabel: Label, e1: Code, comp: Code, e2: Code, thens: Code, elses: Code) extends Code




  case class Call(procedure: Procedure, arguments: Seq[Code], isTail: Boolean = false) extends Code {
    assert(procedure.parameters.size == arguments.size)
  }


  def call(procedure: Procedure, arguments: Code*): Call = Call(procedure, arguments)



  class Procedure(val name: String,
                  val parameters: Seq[Variable],
                  val outer: Option[Procedure] = None) {

    lazy val depth: Int = {
        if outer == null then 0
        else outer match {
          case None => 0
          case Some(proc) => proc.depth + 1
        }
    }


    val paramPtr: Variable = new Variable(s"param pointer of $name", isPointer = true)


    val dynamicLink: Variable = new Variable(s"dynamic link of $name", isPointer = true)


    val savedPC: Variable = new Variable(s"saved pc of $name")


    val staticLink: Variable = new Variable(s"static link of $name", isPointer = true)


    val label = new Label(s"procedure $name")


    var code: Code = null

    override def toString = s"Procedure($name)"
  }


  case class CallClosure(closure: Code, arguments: Seq[Code], parameters: Seq[Variable], isTail: Boolean = false)
    extends Code


  case class Closure(procedure: Procedure) extends Code


  val codePrinter = pprint.copy(additionalHandlers = {
    case word: Word => pprint.Tree.Literal(word.toString)
  })


  val disassemblingCodePrinter = pprint.copy(additionalHandlers = {
    case word: Word => pprint.Tree.Literal(pprint.Util.literalize(Debugger.disassemble(word)))
  })
}
