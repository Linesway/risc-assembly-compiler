package standalone.compiler

import ProgramRepresentation.*
import CodeBuilders.*
import standalone.runtime.*
import standalone.compiler.Assembler.*

object ProcedureExamples {

  lazy val printProcedure: Procedure = {
    val param = new Variable("param")
    val procedure = new Procedure("printProcedure", Seq(param))
    procedure.code = block(
      // You may add code here before and/or after `ProgramExamples.printIntegerCode`. If you do not want any
      // code before or after, replace the ??? with an empty `block()`.
      read(Reg(1), param),
      ProgramExamples.printIntegerCode
    )
    procedure
  }


  lazy val printArray: Procedure = {
    val array = new Variable("array")
    val length = new Variable("length")
    val i = new Variable("i")
    val procedure = new Procedure("printArray", Seq(array, length))
    val temp = new Variable("temp")
    procedure.code = Scope(Seq(i, temp), block(
      write(i, Reg.zero),
      whileLoop(read(Reg.result, i), ltCmp, read(Reg.result, length), block(
        assign(temp, read(Reg.result, array)),

        assign(temp, binOp(read(Reg.result, temp), plus, read(Reg.result, i))),
        assign(temp, binOp(read(Reg.result, temp), plus, read(Reg.result, i))),
        assign(temp, binOp(read(Reg.result, temp), plus, read(Reg.result, i))),
        assign(temp, binOp(read(Reg.result, temp), plus, read(Reg.result, i))),

        call(printProcedure, deref(read(Reg.result, temp))),
        assign(i, binOp(read(Reg.result, i), plus, block(LIS(Reg.result), Word(encodeUnsigned(1)))))
      ))
    ))
    procedure
  }


  lazy val treeHeight: Seq[Procedure] = {
    val array = new Variable("array")
    val length = new Variable("length")
    val treeHeight = new Procedure("treeHeight", Seq(array, length))

    val currentArray = new Variable("currentArray")
    val countTreeHeight = new Procedure("countTreeHeight", Seq(array, length, currentArray))

    treeHeight.code = block(
      Comment("Begin treeHeight and call countTreeHeight"),
      call(countTreeHeight, read(Reg.result, array), read(Reg.result, length), read(Reg.result, array))
    )

    val temp = new Variable("temp")
    val leftHeight = new Variable("leftHeight")
    val rightHeight = new Variable("rightHeight")

    countTreeHeight.code = Scope(Seq(leftHeight, rightHeight, temp), block (
      Comment("Begin call Count Tree Height"),
      read(Reg.result, currentArray),
      Comment("currentArray has been put into reg.result above for debugging"),

      Comment("Set Temp to Left Child Array Offset"),
      assign(temp, binOp(read(Reg.result, currentArray), plus, block(LIS(Reg.result), Word(encodeSigned(4L))))),
      Comment("Check if left child Array Offset is -1"),
      ifStmt(deref(read(Reg.result, temp)), neCmp, block(LIS(Reg.result), Word(encodeSigned(-1))), block(
        Comment("Left child array offset is non-negative, Set Temp to Left Child Array Address"),
        assign(temp, binOp(read(Reg.result, array), plus, binOp(deref(read(Reg.result, temp)), times, block(LIS(Reg.result), Word(encodeSigned(4L)))))),
        Comment("Call treeHeight on Left Child"),
        call(countTreeHeight, read(Reg.result, array), read(Reg.result, length), read(Reg.result, temp)),
        Comment("Write left treeHeight Reg.result to leftHeight variable"),
        write(leftHeight, Reg.result),

      ), block(
        Comment("Left child array offset is -1, write zero to left height"),
        write(leftHeight, Reg.zero)
      )),

      read(Reg.result, currentArray),
      Comment("after left, array has been put into reg.result above for debugging"),

      Comment("Set Temp to Right Child Array Offset"),
      assign(temp, binOp(read(Reg.result, currentArray), plus, block(LIS(Reg.result), Word(encodeSigned(8L))))),
      Comment("Check if Right child Array Offset is -1"),
      ifStmt(deref(read(Reg.result, temp)), neCmp, block(LIS(Reg.result), Word(encodeSigned(-1))), block(
        Comment("Right child array offset is non-negative, Set Temp to Right Child Array Address"),
        assign(temp, binOp(read(Reg.result, array), plus, binOp(deref(read(Reg.result, temp)), times, block(LIS(Reg.result), Word(encodeSigned(4L)))))),
        Comment("Call treeHeight on Right Child"),
        call(countTreeHeight, read(Reg.result, array), read(Reg.result, length), read(Reg.result, temp)),
        Comment("Write Right treeHeight Reg.result to RightHeight variable"),
        write(rightHeight, Reg.result),

      ), block(
        Comment("Right child array offset is -1, write zero to right height"),
        write(rightHeight, Reg.zero)
      )),

      Comment("Get Max Height from left and right children"),
      ifStmt(read(Reg.result, leftHeight), gtCmp, read(Reg.result, rightHeight), block(
        Comment("left height is greater than right"),
        assign(temp, read(Reg.result, leftHeight))), block(
        Comment("right height is greater than left"),
        assign(temp, read(Reg.result, rightHeight)))
      ),

      Comment("add one to temp as final height to return"),
      assign(temp, binOp(read(Reg.result, temp), plus, block(LIS(Reg.result), Word(encodeSigned(1))))),

      Comment("return temp to Reg.result as final height"),
      read(Reg.result, temp)
    ))


    Seq(treeHeight, countTreeHeight)
  }
}
