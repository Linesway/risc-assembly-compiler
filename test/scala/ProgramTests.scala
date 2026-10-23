import standalone.compiler.*
import standalone.compiler.ProgramExamples.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.MemoryManagement.*
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite


class ProgramTests extends AnyFunSuite {

  test("IfStmt sets Reg(3) correctly") {
    // Define two expressions
    val e1 = block(
      LIS(Reg.result),
      Word(encodeSigned(20))
    )
    val e2 = block(
      LIS(Reg.result),
      Word(encodeSigned(10))
    )

    // Define then and else blocks
    val thens = block(
      LIS(Reg(3)),
      Word(encodeSigned(123)) // if e1 < e2
    )
    val elses = block(
      LIS(Reg(3)),
      Word(encodeSigned(456)) // if e1 >= e2
    )

    // Construct IfStmt
    val ifCode = ifStmt(e1, ltCmp, e2, thens, elses)

    // Eliminate IfStmt
    val transformed = eliminateIfStmts(ifCode)

    // Compile to MachineCode
    val compiled = compileStructured(transformed)

    // Run the CPU with empty memory
    val initialState = MachineRunner.setMem(compiled.words)
    val finalState = CPU.run(initialState)

    // Check the output
    println(Assembler.decodeSigned(finalState.reg(3))) // should output 456
  }

  test("whileLoop increments until 5") {
    // variable i
    val i = new Variable("i", false)

    // Code: i = 0; while(i < 5) { i = i + 1; }  -> result in Reg(3)
    val code = Scope(Seq(i), block(
      // i = 0
      write(i, Reg.zero),

      // while (i < 5)
      whileLoop(
        read(Reg.result, i), // e1
        ltCmp, // comparator
        block( // e2 = 5
          LIS(Reg.result),
          Word(encodeSigned(5))
        ),
        block( // body: i = i + 1
          read(Reg.scratch, i),
          LIS(Reg.result),
          Word(encodeSigned(1)),
          plus,
          write(i, Reg.result)
        )
      ),

      // store final i into Reg(3) so we can check it
      read(Reg(3), i),

      JR(Reg(31))
    ))

    val compiled = compileStructured(code)
    val initialState = MachineRunner.setMem(compiled.words)
    val finalState = CPU.run(initialState)

    println(decodeSigned(finalState.reg(3))) // should output 5
  }

  def const(i: Int) = {
    block(LIS(Reg.result), Word(encodeSigned(i)))
  }

  test("arrayScala") {
    val array = Array(2, 4, 6, 8)
    println(array(2))
  }
  test("arrayLacs") {
    val array = Seq[Word](
      Word(encodeSigned(2)),
      Word(encodeSigned(4)),
      Word(encodeSigned(6)),
      Word(encodeSigned(8)),
    )
    val arrayAddr = new Variable("arrayAddr")
    val size = new Variable("size")
    val code = Scope(Seq(arrayAddr, size), block(
      write(arrayAddr, Reg(1)),
      write(size, Reg(2)),

      // *(arrayAddr + 2*4)
      deref(binOp(read(Reg.result, arrayAddr), plus,
        binOp(const(2), times, const(4))
      ))
    ))
    val machineCode = compileStructured(code)
    //    disassemblingCodePrinter.pprintln(machineCode)
    val finalState = ProgramExamples.loadAndRunArray(machineCode, array)
    println(decodeSigned(finalState.reg(3)))
  }

  test("arrayMaximum finds the correct maximum") {
    // Example array: [3, 7, 2, 5]
    val array = Seq(
      Word(encodeSigned(3)),
      Word(encodeSigned(7)),
      Word(encodeSigned(2)),
      Word(encodeSigned(5))
    )
    // Load and run arrayMaximum
    val finalState = ProgramExamples.loadAndRunArray(arrayMaximum, array)

    // Print result
    println(decodeSigned(finalState.reg(3))) // should output 7

    // Assert
    assert(decodeSigned(finalState.reg(3)) == 7)
  }

  test("outputLetters A BC")
  {
    val array = Seq(
      Word(encodeSigned(1L)),
      Word(encodeSigned(0L)),
      Word(encodeSigned(2L)),
      Word(encodeSigned(3L))
    )

    val printed: String = CPU.captureOutput {
      val finalState = ProgramExamples.loadAndRunArray(outputLetters, array)
    }
    println(printed)
  }

  test("print integer -7523")
  {
    val printed: String = CPU.captureOutput {
      val finalState = ProgramExamples.loadAndRun(printInteger, Word(encodeSigned(2147483647L)))
    }
    println(printed)
  }
}
