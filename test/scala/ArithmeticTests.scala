import standalone.compiler.*
import standalone.compiler.Assembler.*
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite


class ArithmeticTests extends AnyFunSuite {

  val p1 = Seq[Word] {
    LIS(Reg(1))
    Word(encodeUnsigned(42))
    LIS(Reg(1))
    Word(encodeUnsigned(100))
    JR(Reg(31))
  }

  test("printp1") {
    println(p1)
    codePrinter.pprintln(p1)
    disassemblingCodePrinter.pprintln(p1)
  }

  def constant(r: Reg, i: Int) = block (
    LIS(Reg(1)),
    Word(encodeUnsigned(i))
  )

  val p2 = block (
    constant(Reg(1), 42),
    constant(Reg(2), 100),
    JR(Reg(31))
  )

  test("printp2") {
    disassemblingCodePrinter.pprintln(p2)
  }

  val a = new Label("a")
  val p3 = block (
    LIS(Reg(2)),
    Use(a),
    Comment("a comment"),
    constant(Reg(1), 42),
    Define(a),
    JR(Reg(31))
  )
  test("printp3")
  {
    disassemblingCodePrinter.pprintln(p3)
    val machineLang = eliminateLabels(eliminateComments(eliminateBlocks(p3)))
    disassemblingCodePrinter.pprintln(machineLang)
    codePrinter.pprintln(machineLang)

    val startState = MachineRunner.setMem(machineLang)
    val endState = CPU.run(startState)
    println(endState)

    val debugTable = createDebugTable(eliminateBlocks(p3))
    codePrinter.pprintln(debugTable)
    val endState2 = Debugger.debug(startState)
    codePrinter.pprintln(debugTable)
  }
}
