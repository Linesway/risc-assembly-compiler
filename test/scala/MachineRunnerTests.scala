import standalone.compiler.{MachineRunner, Assembler}
import standalone.runtime.Word
import org.scalatest.funsuite.AnyFunSuite


class MachineRunnerTests extends AnyFunSuite {
  test("decodeUnsigned") {

    println("You can print output from your tests.")


    assert(1 + 1 == 2, "1 + 1 did not equal 2.")

    println(Assembler.decodeUnsigned(Seq(false)))
    println(Assembler.decodeUnsigned(Seq(false, true, false, false)))
    println(Assembler.decodeUnsigned(Seq(true, true, true, false, true)))
    println(Assembler.decodeSigned(Seq(false, true, true, false, true)))
    println(Assembler.decodeSigned(Seq(true, true, true, false, true)))

    println(Assembler.encodeUnsigned(2, 4))
    println(Assembler.encodeUnsigned(10, 4))
    println(Assembler.encodeSigned(10, 10))
    println(Assembler.encodeSigned(-5, 10))
  }

  test("add134 adds registers correctly") {
    val r1: Word = Word(Assembler.encodeUnsigned(5, 32))
    val r3: Word = Word(Assembler.encodeUnsigned(10, 32))
    val state = MachineRunner.loadAndRun(MachineRunner.add134, r1, r3)

    assert(Assembler.decodeUnsigned(state.reg(4)) == 10, "Register 3 should contain r1 + r1")
  }

  test("maximum computes correct maximum") {
    val r1: Word = Word(Assembler.encodeUnsigned(8, 32))
    val r2: Word = Word(Assembler.encodeUnsigned(12, 32))
    val state = MachineRunner.loadAndRun(MachineRunner.maximum, r1, r2)

    assert(Assembler.decodeUnsigned(state.reg(3)) == 12, "Register 3 should contain the maximum value")
  }
  test("maximum computes correct maximum 2") {
    val r1: Word = Word(Assembler.encodeUnsigned(12, 32))
    val r2: Word = Word(Assembler.encodeUnsigned(8, 32))
    val state = MachineRunner.loadAndRun(MachineRunner.maximum, r1, r2)

    assert(Assembler.decodeUnsigned(state.reg(3)) == 12, "Register 3 should contain the maximum value")
  }
}
