package standalone.compiler

import Assembler.*
import ProgramRepresentation.*
import Transformations.*
import standalone.runtime.*

object ArithmeticExamples {



  lazy val maximum: Seq[Word] = {
    val label = new Label("label")
    val code = Seq[Code](
      SLT(Reg(4), Reg(1), Reg(2)), //1 if Reg(1) < Reg(2), 0 otherwise
      beq(Reg(4), Reg(0), label), //skip two lines if Reg(1) > Reg(2)

      //we know that Reg(2) is the max
      ADD(Reg(3), Reg(2), Reg(0)),
      JR(Reg(31)),

      Define(label),

      //we know that Reg(1) is the max
      ADD(Reg(3), Reg(1), Reg(0)),
      JR(Reg(31))
    )
    eliminateLabels(code)
  }


  lazy val maximumUnsigned: Seq[Word] = {
    val label = new Label("label")
    val code = Seq[Code](
      SLTU(Reg(4), Reg(1), Reg(2)), //1 if Reg(1) < Reg(2), 0 otherwise
      beq(Reg(4), Reg(0), label), //skip two lines if Reg(1) > Reg(2)

      //we know that Reg(2) is the max
      ADD(Reg(3), Reg(2), Reg(0)),
      JR(Reg(31)),

      Define(label),

      //we know that Reg(1) is the max
      ADD(Reg(3), Reg(1), Reg(0)),
      JR(Reg(31))
    )
    eliminateLabels(code)
  }

}
