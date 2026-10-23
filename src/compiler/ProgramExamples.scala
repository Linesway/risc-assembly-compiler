package standalone.compiler

import standalone.runtime.*
import Assembler.*
import Transformations.*
import ProgramRepresentation.*
import CodeBuilders.*
import MachineRunner.*

object ProgramExamples {

  def loadAndRun(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero, debug: Boolean = false): State = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    if(debug) Debugger.debug(initialState, code.debugTable)
    else CPU.run(initialState)
  }

  def interactiveDebugger(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero): Debugger.InteractiveDebugger = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    Debugger.interactive(initialState, code.debugTable)
  }


  def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
    val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
    val arrayLength: Word = Word(encodeUnsigned(array.size))
    val loadedCode: State = setMem(code)
    val loadedArray: State = setMem(array, loadedCode, arrayAddress)
    loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
  }


  def loadAndRunArray(code: MachineCode, array: Seq[Word], debug: Boolean = false): State = {
    val state = loadCodeAndArray(code.words, array)
    if(debug) Debugger.debug(state, code.debugTable)
    else CPU.run(state)
  }


  lazy val lastElement: MachineCode = {
    val code: Code = {
      val emptyLabel = new Label("emptyLabel")
      block(
        ADD(Reg.scratch, Reg(2), Reg.zero),
        ADD(Reg.result, Reg.zero, Reg.zero),
        gtCmp(emptyLabel),

        LIS(Reg.scratch),
        Word(encodeSigned(1L)),
        SUB(Reg.scratch, Reg(2), Reg.scratch),
        LIS(Reg.result),
        Word(encodeSigned(4L)),
        times,

        ADD(Reg.scratch, Reg.result, Reg(1)),
        LW(Reg(3), 0, Reg.scratch),
        JR(Reg(31)),

        Define(emptyLabel),
        LIS(Reg(3)),
        Word(encodeSigned(-1L)),
        JR(Reg(31))
      )
    }
    compileStructured(code)
  }


  lazy val arrayMaximum: MachineCode = {

    val code: Code = {
      val i = new Variable("i", false)
      val maxElement = new Variable("maxElement", false)
      val currentElement = new Variable("currentElement", false)
      Scope(Seq(i, maxElement, currentElement), block(
        write(i, Reg.zero),

        LW(Reg.result, 0, Reg(1)),
        write(maxElement, Reg.result),

        whileLoop(read(Reg.result, i), ltCmp, ADD(Reg.result, Reg(2), Reg.zero),
          block(
            read(Reg.result, i),
            LIS(Reg.scratch),
            Word(encodeSigned(4L)),
            times,

            ADD(Reg.result, Reg.result, Reg(1)),
            LW(Reg.result, 0, Reg.result),
            write(currentElement, Reg.result),
            ifStmt(read(Reg.result, currentElement), gtCmp, read(Reg.result, maxElement),
              block(
                read(Reg.result, currentElement),
                write(maxElement, Reg.result)
              ),
              block()
            ),
            read(Reg.result, i),
            LIS(Reg.scratch),
            Word(encodeSigned(1L)),
            plus,
            write(i, Reg.result)
          )
        ),
        read(Reg(3), maxElement),
        JR(Reg(31))
      ))
    }
    compileStructured(code)
  }



  lazy val outputLetters: MachineCode = {

    val code: Code = {
      val i = new Variable("i", false)
      val currentElement = new Variable("currentElement", false)
      val endLabel = new Label("endLabel")

      Scope(Seq(i, currentElement), block(
        ADD(Reg.scratch, Reg(2), Reg.zero),
        ADD(Reg.result, Reg.zero, Reg.zero),
        gtCmp(endLabel),

        write(i, Reg.zero),
        whileLoop(read(Reg.result, i), ltCmp, ADD(Reg.result, Reg(2), Reg.zero), block(
          read(Reg.result, i),
          LIS(Reg.scratch),
          Word(encodeSigned(4L)),
          times,

          ADD(Reg.result, Reg.result, Reg(1)),
          LW(Reg.result, 0, Reg.result),
          write(currentElement, Reg.result),

          ifStmt(read(Reg.result, currentElement), eqCmp, ADD(Reg.result, Reg.zero, Reg.zero),
            block(
              LIS(Reg.scratch),
              Word(encodeSigned(32L)),
              LIS(Reg.result),
              CPU.printAddr,
              SW(Reg.scratch, 0, Reg.result)
            ),
            block(
              read(Reg.result, currentElement),
              LIS(Reg.scratch),
              Word(encodeSigned(64L)),
              ADD(Reg.result, Reg.scratch, Reg.result),
              LIS(Reg.scratch),
              CPU.printAddr,
              SW(Reg.result, 0, Reg.scratch)
            )
          ),
          read(Reg.result, i),
          LIS(Reg.scratch),
          Word(encodeSigned(1L)),
          plus,
          write(i, Reg.result)
        )),

        Define(endLabel),
        JR(Reg(31))
      ))
    }
    compileStructured(code)
  }



  lazy val printIntegerCode: Code = {
    val currentElement = new Variable("currentElement", false)
    val printIntegerCodeLabel = new Label("printIntegerCode")
    val printIntegerEndLabel = new Label("printIntegerEndLabel")

    Scope(Seq(currentElement), block(
      Comment("i hate memorymanagement.scala"),

      write(currentElement, Reg(1)),

      ifStmt(read(Reg.result, currentElement), eqCmp, ADD(Reg.result, Reg.zero, Reg.zero), block(
        LIS(Reg.scratch),
        CPU.printAddr,
        LIS(Reg.result),
        Word(encodeSigned(48L)),
        SW(Reg.result, 0, Reg.scratch),  // print 0
      ), block()),

      ifStmt(read(Reg.result, currentElement), ltCmp, ADD(Reg.result, Reg.zero, Reg.zero), block(
        LIS(Reg.scratch),
        CPU.printAddr,
        LIS(Reg.result),
        Word(encodeSigned(45L)),
        SW(Reg.result, 0, Reg.scratch),  // print negative sign
//
//        //negate currentElement
//        SUB(Reg.result, Reg.zero, Reg(1)),
//        write(currentElement, Reg.result)
      ), block()),

      LIS(Reg.scratch),
      Word(encodeSigned(4L)),
      SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      SW(Reg.link, 0, Reg.stackPointer), //save ra

      LIS(Reg.targetPC),
      Use(printIntegerCodeLabel),
      JALR(Reg.targetPC), //recurse

      LW(Reg.link, 0, Reg.stackPointer),
      LIS(Reg.scratch),
      Word(encodeSigned(4L)),
      ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),

      LIS(Reg.scratch),
      CPU.printAddr,
      LIS(Reg.result),
      Word(encodeSigned(10L)),
      SW(Reg.result, 0, Reg.scratch),  // print newline

      Comment("i hate transformations.scala"),
      LIS(Reg.scratch),
      Use(printIntegerEndLabel),
      JR(Reg.scratch),

      Define(printIntegerCodeLabel),

      ifStmt(read(Reg.result, currentElement), neCmp, ADD(Reg.result, Reg.zero, Reg.zero),
        block(
          LIS(Reg.scratch),
          Word(encodeSigned(8L)),
          SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
          SW(Reg.link, 4, Reg.stackPointer), //save ra

          read(Reg.result, currentElement),
          ADD(Reg.scratch, Reg.result, Reg.zero),
          LIS(Reg.result),
          Word(encodeSigned(10L)),
          remainder, //get remainder from 10 division
          SW(Reg.result, 0, Reg.stackPointer), //save remainder to stack pointer

          read(Reg.result, currentElement),
          ADD(Reg.scratch, Reg.result, Reg.zero),
          LIS(Reg.result),
          Word(encodeSigned(10L)),
          divide,
          write(currentElement, Reg.result), // divide and write currentElement by 10

          LIS(Reg.targetPC),
          Use(printIntegerCodeLabel),
          JALR(Reg.targetPC), //recurse

          //get remainder back from stack pointer
          ifStmt(LW(Reg.result, 0, Reg.stackPointer), ltCmp, ADD(Reg.result, Reg.zero, Reg.zero),
            block(
              LW(Reg.result, 0, Reg.stackPointer),
              SUB(Reg.result, Reg.zero, Reg.result),
            ),
            LW(Reg.result, 0, Reg.stackPointer)
          ), //abs

          LIS(Reg.scratch),
          Word(encodeSigned(48L)),
          ADD(Reg.result, Reg.scratch, Reg.result), //convert to ascii

          LIS(Reg.scratch),
          CPU.printAddr,
          SW(Reg.result, 0, Reg.scratch), //print ascii

          LW(Reg.link, 4, Reg.stackPointer),
          LIS(Reg.scratch),
          Word(encodeSigned(8L)),
          ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
          JR(Reg.link) //restore return address and stack pointer and return
        ),
        block(
          JR(Reg.link)
        ) // return to callee to stop recursion if currentElement is not gt 0
      ),
      Define(printIntegerEndLabel)
    ))
  }
  lazy val printInteger: MachineCode = compileStructured(printIntegerCode)
}
