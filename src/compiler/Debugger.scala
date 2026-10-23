package standalone.compiler

import standalone.runtime.*
import standalone.Utils.*
import State.*
import Assembler.*
import standalone.compiler.ProgramRepresentation.*

import scala.annotation.tailrec


object Debugger {

  def disassemble(instruction: Word): String = {
    def invalidInstruction = decodeSigned(instruction).toString

    val List(op, sBits, tBits, iBits) = instruction.splitAt(List(6, 5, 5))
    val s = decodeUnsigned(sBits)
    val t = decodeUnsigned(tBits)
    val i = decodeSigned(iBits)
    op match {
      case Bits("000000") =>
        val List(dBits, zeros, function) = iBits.splitAt(List(5, 5))
        val d = decodeUnsigned(dBits)
        if (zeros != Bits("00000")) invalidInstruction
        else function match {
          case Bits("100000") => s"add $$$d, $$$s, $$$t"
          case Bits("100010") => s"sub $$$d, $$$s, $$$t"
          case Bits("011000") if d == 0 => s"mult $$$s, $$$t"
          case Bits("011001") if d == 0 => s"multu $$$s, $$$t"
          case Bits("011010") if d == 0 => s"div $$$s, $$$t"
          case Bits("011011") if d == 0 => s"divu $$$s, $$$t"
          case Bits("010000") if s == 0 && t == 0 => s"mfhi $$$d"
          case Bits("010010") if s == 0 && t == 0 => s"mflo $$$d"
          case Bits("010100") if s == 0 && t == 0 => s"lis $$$d"
          case Bits("101010") => s"slt $$$d, $$$s, $$$t"
          case Bits("101011") => s"sltu $$$d, $$$s, $$$t"
          case Bits("001000") if t == 0 && d == 0 => s"jr $$$s"
          case Bits("001001") if t == 0 && d == 0 => s"jalr $$$s"
          case _ => invalidInstruction
        }

      case Bits("100011") => s"lw $$$t, $i($$$s)"
      case Bits("101011") => s"sw $$$t, $i($$$s)"
      case Bits("000100") => s"beq $$$s, $$$t, $i"
      case Bits("000101") => s"bne $$$s, $$$t, $i"
      case _ => invalidInstruction
    }
  }


  type DebugTable = Map[Word, Seq[Comment|Label]]


  def debug(state: State, debugTable: DebugTable = Map.empty): State = {
    val id = new InteractiveDebugger(state, debugTable) { override def isAtBreakPoint: Boolean = false }
    id.run
    id.state
  }


  def interactive(state: State, debugTable: DebugTable = Map.empty): InteractiveDebugger =
    new InteractiveDebugger(state, debugTable)

  class InteractiveDebugger(initialState: State, debugTable: DebugTable) {
    var state: State = initialState
    def pc = state.reg(PC)
    def isDoneExecuting = pc == CPU.terminationPC


    def isAtBreakPoint: Boolean = debugTable.getOrElse(pc, Seq()).exists {
      case Comment(msg) => msg.startsWith("BREAK")
      case _ => false
    }


    def showCurrentState: Unit = {
      def isAtLISInstruction: Boolean = disassemble(state.mem(pc)).startsWith("lis ")

      regs(3, 4, 29, 5, 30)

      if (debugTable.isDefinedAt(pc)) println(debugTable(pc).mkString("\n"))

      if(isDoneExecuting) println("EXECUTION FINISHED")
      else mem(pc, words = if (isAtLISInstruction) 2 else 1)
    }


    def regs: Unit = regs(1 to 31 :_*)
    def regs(regsToPrint: Int*): Unit = {
      val string = regsToPrint.map(reg => s"$reg: ${decodeSigned(state.reg(reg))}   ").foldLeft("")(_ + _)
      println(string)
    }


    def mem(address: String, words: Int = 1): Unit = mem(Word(address), words)
    def mem(address: Word): Unit = mem(address, 1)
    def mem(address: Word, words: Int): Unit = dumpMem(state, address, words)


    def run: Unit = run(Integer.MAX_VALUE)
    @tailrec final def run(maxSteps: Int): Unit = {
      if(maxSteps > 0 && !isDoneExecuting) {
        state = CPU.step(state)
        showCurrentState
        if(!isAtBreakPoint) run(maxSteps - 1)
      }
    }


    def step: Unit = run(1)

    showCurrentState
  }


  def prettyPrintWord(w: Word): String = w.toString().sliding(8, 8).mkString(" ")


  def dumpMem(state: State, address: Word, words: Int = 1): Unit = {
    if (words > 0) {
      val word = state.mem(address)
      println(s"${prettyPrintWord(address)}: ${prettyPrintWord(word)} // ${disassemble(word)}")
      dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words - 1)
    }
  }
}
