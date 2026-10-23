package standalone.compiler

import ProgramRepresentation.*
import Assembler.*
import standalone.runtime.*


object CodeBuilders {





  def binOp(e1: Code, op: Code, e2: Code): Code = {

    val tempVar = new Variable("tempVar", false)
      Scope(Seq(tempVar),
        block(
          e1,
          write(tempVar, Reg.result),
          e2,
          read(Reg.scratch, tempVar),
          op
        )
      )
  }


  lazy val plus: Code = block(ADD(Reg.result, Reg.scratch, Reg.result))
  lazy val minus: Code = block(SUB(Reg.result, Reg.scratch, Reg.result))
  lazy val times: Code = block(MULT(Reg.scratch, Reg.result), MFLO(Reg.result))
  lazy val divide: Code = block(DIV(Reg.scratch, Reg.result), MFLO(Reg.result))
  lazy val remainder: Code = block(DIV(Reg.scratch, Reg.result), MFHI(Reg.result))
  lazy val divideUnsigned: Code = block(DIVU(Reg.scratch, Reg.result), MFLO(Reg.result))
  lazy val remainderUnsigned: Code = block(DIVU(Reg.scratch, Reg.result), MFHI(Reg.result))


  def eqCmp(label: Label): Code = block(bne(Reg.scratch, Reg.result, label))
  def neCmp(label: Label): Code = block(beq(Reg.scratch, Reg.result, label))
  def ltCmp(label: Label): Code = block(SLT(Reg.scratch, Reg.scratch, Reg.result), beq(Reg.scratch, Reg.zero, label))
  def gtCmp(label: Label): Code = block(SLT(Reg.scratch, Reg.result, Reg.scratch), beq(Reg.scratch, Reg.zero, label))
  def leCmp(label: Label): Code = block(SLT(Reg.scratch, Reg.result, Reg.scratch), bne(Reg.scratch, Reg.zero, label))
  def geCmp(label: Label): Code = block(SLT(Reg.scratch, Reg.scratch, Reg.result), bne(Reg.scratch, Reg.zero, label))
  def gtUnsignedCmp(label: Label): Code = block(SLTU(Reg.scratch, Reg.result, Reg.scratch), beq(Reg.scratch, Reg.zero, label))


  def deref(expr: Code): Code = {
    block(
      expr,
      LW(Reg.result, 0, Reg.result)
    )
  }


  def assignToAddr(target: Code, expr: Code): Code = {
    val tempVar = new Variable("tempVar", false)
    Scope(Seq(tempVar), block(
      target,
      write(tempVar, Reg.result),
      expr,
      read(Reg.scratch, tempVar),
      SW(Reg.result, 0, Reg.scratch)
    ))
  }


  def whileLoop(e1: Code, comp: Label=>Code, e2: Code, body: Code): Code = {
    val loopLabel = new Label("loopLabel")
    val endLabel = new Label("endLabel")
    block(
      Define(loopLabel),
      binOp(e1, comp(endLabel), e2),
      body,
      beq(Reg.zero, Reg.zero, loopLabel),
      Define(endLabel)
    )
  }


}
