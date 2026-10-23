import standalone.compiler.*
import standalone.compiler.ProcedureExamples.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.MemoryManagement.Chunk
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite

class ProcedureTests extends AnyFunSuite {
  test("scala") {
    def main(a: Int, b: Int): Int = { a + b }
    println(main(1,2))
  }
  test("lacs") {
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = binOp(read(Reg.result, a), plus, read(Reg.result, b))
    val machineCode = compileProcedures(Seq(main))
    //    disassemblingCodePrinter.pprintln(machineCode)
    val finalState = ProgramExamples.loadAndRun(machineCode, register1 = Word(encodeSigned(1)),
      register2 = Word(encodeSigned(2)))
    println(decodeSigned(finalState.reg(3)))
  }
  def const(i: Int) = {
    block(LIS(Reg.result), Word(encodeSigned(i)))
  }
  test("scala_fact") {
    def main(a: Int, b: Int): Int = { fact(a) }
    def fact(i: Int): Int = {
      val iMinusOne = i-1
      if(i<=0) { 1 } else i*fact(iMinusOne)
    }
    println(main(10,2))
  }
  test("lacs_fact") {
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    val i = new Variable("i")
    val iMinusOne = new Variable("iMinusOne")
    val fact = new Procedure("fact", Seq(i))
    main.code = call(fact, read(Reg.result, a))
    fact.code = Scope(Seq(iMinusOne), block(
      assign(iMinusOne, binOp(read(Reg.result, i), minus, const(1))),
      ifStmt(read(Reg.result, i), leCmp, const(0), const(1),
        binOp(read(Reg.result, i), times, call(fact, read(Reg.result, iMinusOne)))
        //      read(Reg.result, iMinusOne)
      )))
    // test: f(g())  f(f(1))
    // test: reading/writing variables/parameters
    val machineCode = compileProcedures(Seq(main, fact))
    //    disassemblingCodePrinter.pprintln(machineCode)
    val finalState = ProgramExamples.loadAndRun(machineCode, register1 = Word(encodeSigned(10)),
      register2 = Word(encodeSigned(2)), debug=true)
    println(decodeSigned(finalState.reg(3)))
  }
  test("pass_through_two_params") {
    val x = new Variable("x")
    val y = new Variable("y")
    val main = new Procedure("main", Seq(x, y))

    // main just writes x to Reg.result
    main.code = Scope(Seq(), block(
      // copy inputs from registers into variables
      write(x, Reg(1)),
      write(y, Reg(2)),
      // copy x back into Reg.result
      read(Reg.result, x)
    ))

    val machineCode = compileProcedures(Seq(main))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(42)), // x input
      register2 = Word(encodeSigned(99)), // y input
      debug = true
    )

    println(decodeSigned(finalState.reg(3))) // should print 42
  }
  test("nested_two_params") {
    val x = new Variable("x")
    val y = new Variable("y")
    val main = new Procedure("main", Seq(x, y))

    main.code = Scope(Seq(), block(
      // Copy inputs into variables
      write(x, Reg(1)),
      write(y, Reg(2)),

      // Perform nested arithmetic:  ((x + y) * (x + 1))
      assign(x, binOp(
        binOp(read(Reg.result, x), plus, read(Reg.result, y)), // (x + y)
        times,
        binOp(read(Reg.result, x), plus, const(1)) // (x + 1)
      )),

      // Return result
      read(Reg.result, x)
    ))

    val machineCode = compileProcedures(Seq(main))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(2)), // x
      register2 = Word(encodeSigned(3)), // y
      debug = true
    )

    println(decodeSigned(finalState.reg(3))) // expected (2 + 3) * (2 + 1) = 5 * 3 = 15
  }

  test("simple_recursive_two_params") {
    val a = new Variable("a")
    val b = new Variable("b")
    val aMinusOne = new Variable("aMinusOne")

    val main = new Procedure("main", Seq(a, b))
    val ping = new Procedure("ping", Seq(a, b))

    // ping(a, b):
    //   aMinusOne = a - 1
    //   if (a <= 0) return b
    //   else return ping(aMinusOne, b)
    ping.code = Scope(Seq(aMinusOne), block(
      assign(aMinusOne, binOp(read(Reg.result, a), minus, const(1))),
      ifStmt(
        read(Reg.result, a), leCmp, const(0),
        read(Reg.result, b),
        block(
          read(Reg.result, a), // add this — will print current 'a' before recursing
          call(ping, read(Reg.result, aMinusOne), read(Reg.result, b))
        )
      )
    ))

    // main(a, b):
    //   call ping(a, b)
    main.code = block(
      call(ping, read(Reg.result, a), read(Reg.result, b))
    )

    val machineCode = compileProcedures(Seq(main, ping))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(3)), // a = 3
      register2 = Word(encodeSigned(99)), // b = 99
      debug = true
    )

    println(decodeSigned(finalState.reg(3))) // should print 99
  }

  test("print array") {
    // Example array: [3, 7, 2, 5]
    val array = Seq(
      Word(encodeSigned(5)),
    )
    // Load and run arrayMaximum
    val finalState = ProgramExamples.loadAndRunArray(compileProcedures(Seq(printArray, printProcedure)), array, true)
  }

  test("treeHeight test") {
    // Tree structure:
    //        77
    //       /  \
    //     22    -8
    //          /  \
    //        -36   999
    //
    // Expected height: 3

    val array = Seq(
      Word(encodeSigned(77)), // A[0]
      Word(encodeSigned(3)), // A[1]
      Word(encodeSigned(6)), // A[2]
      Word(encodeSigned(22)), // A[3]
      Word(encodeSigned(-1)), // A[4]
      Word(encodeSigned(-1)), // A[5]
      Word(encodeSigned(-8)), // A[6]
      Word(encodeSigned(9)), // A[7]
      Word(encodeSigned(12)), // A[8]
      Word(encodeSigned(-36)), // A[9]
      Word(encodeSigned(-1)), // A[10]
      Word(encodeSigned(-1)), // A[11]
      Word(encodeSigned(999)), // A[12]
      Word(encodeSigned(-1)), // A[13]
      Word(encodeSigned(-1)) // A[14]
    )
    // Load and run arrayMaximum
    val finalState = ProgramExamples.loadAndRunArray(compileProcedures(treeHeight), array, true)
  }
}
