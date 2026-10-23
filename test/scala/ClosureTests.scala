import standalone.compiler.*
import standalone.compiler.ProcedureExamples.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.MemoryManagement.Chunk
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite

class ClosureTests extends AnyFunSuite {

  test("nested procedures test") {



    // Variables
    val a = new Variable("a")
    val b = new Variable("b")
    val x = new Variable("x")
    val y = new Variable("y")
    val z = new Variable("z")

    // Outer procedure
    val outer = new Procedure("outer", Seq(a, b))
    // Inner procedure nested in outer
    val inner = new Procedure("inner", Seq(x, y), Some(outer))

    // Outer procedure code: assign sum of a + b to a local variable z
    val zVar = new Variable("z")
    outer.code = Scope(Seq(zVar), block(
      assign(zVar, binOp(read(Reg.result, a), plus, read(Reg.result, b))),
      // Call inner procedure with z and b
      call(inner, read(Reg.result, zVar), read(Reg.result, b))
    ))

    // Inner procedure code: compute x * y and store in result
    inner.code = Scope(Seq(), block(
      assign(x, binOp(read(Reg.result, x), times, read(Reg.result, y))),
      read(Reg.result, x)
    ))

    // Compile and run
    val machineCode = compileClosures(Seq(outer, inner))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(3)), // a
      register2 = Word(encodeSigned(4)), // b
      debug = true
    )

    // outer: z = a + b = 7
    // inner: x = z * b = 7 * 4 = 28
    assert(decodeSigned(finalState.reg(3)) == 28)
    println(decodeSigned(finalState.reg(3))) // should print 28
  }
  test("simple recursive countdown test") {



    val n = new Variable("n")
    val dummy = new Variable("dummy")
    val i = new Variable("i")
    val iMinusOne = new Variable("iMinusOne")

    val main = new Procedure("main", Seq(n, dummy))
    val countdown = new Procedure("countdown", Seq(i, dummy), Some(main))

    // main calls countdown(n, dummy)
    main.code = block(
      call(countdown, read(Reg.result, n), block(LIS(Reg.result), Word(encodeSigned(0))))
    )

    // countdown(i, dummy):
    // if (i <= 0) return 0 else return countdown(i - 1, dummy)
    countdown.code = Scope(Seq(iMinusOne), block(
      // iMinusOne = i - 1
      assign(iMinusOne,
        binOp(read(Reg.result, i), minus, block(LIS(Reg.result), Word(encodeSigned(1))))
      ),

      // if (i <= 0)
      ifStmt(
        read(Reg.result, i), leCmp, block(LIS(Reg.result), Word(encodeSigned(0))),

        // base case: return 0
        block(
          LIS(Reg.result),
          Word(encodeSigned(0))
        ),

        // recursive case: call countdown(i - 1, dummy)
        block(
          Comment("recursive case executing"),
          call(countdown, read(Reg.result, iMinusOne), block(LIS(Reg.result), Word(encodeSigned(0))))
        )
      )
    ))

    // Compile and run
    val machineCode = compileClosures(Seq(main, countdown))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(3)), // n = 3
      register2 = Word(encodeSigned(0)), // dummy
      debug = true
    )

    // Expected result: 0
    println(decodeSigned(finalState.reg(3))) // should print 0
  }

  test("closure creation and call test with 2 dummy parameters") {



    // Variables
    val dummy1 = new Variable("dummy1")
    val dummy2 = new Variable("dummy2")
    val a = new Variable("a")
    val b = new Variable("b")
    val x = new Variable("x")
    val y = new Variable("y")
    val z = new Variable("z")

    // outer procedure
    val outer = new Procedure("outer", Seq(a, b))
    // inner procedure nested inside outer
    val inner = new Procedure("inner", Seq(x, y), Some(outer))

    // inner code: x * y
    inner.code = Scope(Seq(), block(
      assign(x, binOp(read(Reg.result, x), times, read(Reg.result, y))),
      read(Reg.result, x)
    ))

    // outer code: z = a + b, call inner(z, b)
    val zVar = new Variable("z")
    outer.code = Scope(Seq(zVar), block(
      assign(zVar, binOp(read(Reg.result, a), plus, read(Reg.result, b))),
      CallClosure(
        closure = Closure(inner),
        arguments = Seq(read(Reg.result, zVar), read(Reg.result, b)),
        parameters = inner.parameters
      )
    ))

    // main procedure with exactly 2 parameters (dummy1, dummy2)
    val main = new Procedure("main", Seq(dummy1, dummy2))
    val outerClosure = new Variable("outerClosure")
    main.code = Scope(Seq(outerClosure), block(
      assign(outerClosure, Closure(outer)), // create closure
      CallClosure(
        closure = read(Reg.result, outerClosure),
        arguments = Seq(block(LIS(Reg.result), Word(encodeSigned(3))),
          block(LIS(Reg.result), Word(encodeSigned(4)))), // pass actual a=3, b=4
        parameters = outer.parameters
      )
    ))

    // Compile and run
    val machineCode = compileClosures(Seq(main, outer, inner))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(0)), // dummy1
      register2 = Word(encodeSigned(0)), // dummy2
      debug = true
    )

    // Expected: outer z = 3 + 4 = 7, inner x * y = 7 * 4 = 28
    assert(decodeSigned(finalState.reg(3)) == 28)
    println(decodeSigned(finalState.reg(3))) // should print 28
  }
  test("tail recursive factorial test with dummy param") {



    val n = new Variable("n")
    val acc = new Variable("acc")
    val dummy = new Variable("dummy") // unused, just required
    val nMinusOne = new Variable("nMinusOne")
    val accTimesN = new Variable("accTimesN")

    // main takes 2 params: x and dummy
    val x = new Variable("x")
    val mainDummy = new Variable("mainDummy")
    val main = new Procedure("main", Seq(x, mainDummy))

    // factorial takes 3 params: n, acc, dummy
    val factorial = new Procedure("factorial", Seq(n, acc, dummy), Some(main))

    // main(x, dummy):
    //   call factorial(x, 1, dummy)
    main.code = block(
      call(
        factorial,
        read(Reg.result, x),
        block(LIS(Reg.result), Word(encodeSigned(1))),
        read(Reg.result, mainDummy)
      )
    )

    // factorial(n, acc, dummy):
    factorial.code = Scope(Seq(nMinusOne, accTimesN), block(

      // nMinusOne = n - 1
      assign(nMinusOne,
        binOp(read(Reg.result, n), minus, block(LIS(Reg.result), Word(encodeSigned(1))))
      ),

      // accTimesN = acc * n
      assign(accTimesN,
        binOp(read(Reg.result, acc), times, read(Reg.result, n))
      ),

      ifStmt(
        read(Reg.result, n), leCmp,
        block(LIS(Reg.result), Word(encodeSigned(1))),

        // base case: return acc
        read(Reg.result, acc),

        // recursive *tail call* (does not grow stack)
        call(
          factorial,
          read(Reg.result, nMinusOne),
          read(Reg.result, accTimesN),
          read(Reg.result, dummy)
        )
      )
    ))

    // Compile & run
    val machineCode = compileClosures(Seq(main, factorial))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(5)), // x = 5
      register2 = Word(encodeSigned(999)), // dummy (unused)
      debug = true
    )

    // Expect 120
    assert(decodeSigned(finalState.reg(3)) == 120)
    println("Factorial(5) result: " + decodeSigned(finalState.reg(3)))
  }
  test("deep mutual tail recursion no-closure test") {

    val dummy = new Variable("dummy")

    val nEven = new Variable("nEven")
    val nOdd = new Variable("nOdd")

    val tmpEven = new Variable("tmpEven")
    val tmpOdd = new Variable("tmpOdd")

    val isEven = new Procedure("isEven", Seq(nEven, dummy))
    val isOdd = new Procedure("isOdd", Seq(nOdd, dummy))

    // isEven(n, d):
    // if n <= 0 return 1 else tail-call isOdd(n-1, d)
    isEven.code = Scope(Seq(tmpEven), block(
      assign(tmpEven,
        binOp(read(Reg.result, nEven), minus, block(LIS(Reg.result), Word(encodeSigned(1))))
      ),
      ifStmt(
        read(Reg.result, nEven), leCmp, block(LIS(Reg.result), Word(encodeSigned(0))),
        // Base: return 1
        block(LIS(Reg.result), Word(encodeSigned(1))),
        // Tail call
        call(isOdd, read(Reg.result, tmpEven), read(Reg.result, dummy))
      )
    ))

    // isOdd(n, d):
    // if n <= 0 return 0 else tail-call isEven(n-1, d)
    isOdd.code = Scope(Seq(tmpOdd), block(
      assign(tmpOdd,
        binOp(read(Reg.result, nOdd), minus, block(LIS(Reg.result), Word(encodeSigned(1))))
      ),
      ifStmt(
        read(Reg.result, nOdd), leCmp, block(LIS(Reg.result), Word(encodeSigned(0))),
        // Base: return 0
        block(LIS(Reg.result), Word(encodeSigned(0))),
        // Tail call
        call(isEven, read(Reg.result, tmpOdd), read(Reg.result, dummy))
      )
    ))

    // main(dummy1, dummy2):
    // return isEven(2000, dummy2)
    val mainDummy1 = new Variable("mainDummy1")
    val mainDummy2 = new Variable("mainDummy2")
    val main = new Procedure("main", Seq(mainDummy1, mainDummy2))
    main.code = block(
      call(isEven, block(LIS(Reg.result), Word(encodeSigned(2000))), read(Reg.result, mainDummy2))
    )

    val machineCode = compileClosures(Seq(main, isEven, isOdd))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(0)),
      register2 = Word(encodeSigned(999)), // dummy
      debug = false
    )

    val result = decodeSigned(finalState.reg(3))
    println("isEven(2000) result = " + result)
    assert(result == 1)
  }
  test("false tail call test: result used after call") {

    val dummy = new Variable("dummy")
    val n = new Variable("n")
    val nMinusOne = new Variable("nMinusOne")

    val f = new Procedure("f", Seq(n, dummy))
    val tmp = new Variable("tmp")

    // f(n, dummy): if n <= 0 return 0 else f(n-1, dummy)
    f.code = Scope(Seq(nMinusOne), block(
      assign(nMinusOne, binOp(read(Reg.result, n), minus, block(LIS(Reg.result), Word(encodeSigned(1))))),
      ifStmt(
        read(Reg.result, n), leCmp, block(LIS(Reg.result), Word(encodeSigned(0))),
        block(LIS(Reg.result), Word(encodeSigned(0))),
        call(f, read(Reg.result, nMinusOne), read(Reg.result, dummy))
      )
    ))

    val mainDummy1 = new Variable("mainDummy1")
    val mainDummy2 = new Variable("mainDummy2")
    val x = new Variable("x")
    val main = new Procedure("main", Seq(mainDummy1, mainDummy2))

    // main: x = f(5,dummy); return x + 1
    val xPlusOne = new Variable("xPlusOne")
    main.code = Scope(Seq(x, xPlusOne), block(
      assign(x, call(f, block(LIS(Reg.result), Word(encodeSigned(5))), read(Reg.result, mainDummy2))),
      assign(xPlusOne, binOp(read(Reg.result, x), plus, block(LIS(Reg.result), Word(encodeSigned(1))))),
      read(Reg.result, xPlusOne)
    ))

    val machineCode = compileClosures(Seq(main, f))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(0)),
      register2 = Word(encodeSigned(999)),
      debug = true
    )

    val result = decodeSigned(finalState.reg(3))
    println("False tail call test result = " + result)
    assert(result == 1)
  }
  test("complicated multi-level closure test") {



    // Variables
    val dummy1 = new Variable("dummy1")
    val dummy2 = new Variable("dummy2")
    val a = new Variable("a")
    val b = new Variable("b")
    val c = new Variable("c")

    // Procedures
    val outer = new Procedure("outer", Seq(a))
    val middle = new Procedure("middle", Seq(b), Some(outer))
    val inner = new Procedure("inner", Seq(c), Some(middle))

    // Inner: return c * 2
    inner.code = Scope(Seq(), block(
      assign(c, binOp(read(Reg.result, c), times, block(LIS(Reg.result), Word(encodeSigned(2))))),
      read(Reg.result, c)
    ))

    val innerClosure = new Variable("innerClosure")

    // Middle: create inner closure, call inner(b * 2)
    middle.code = Scope(Seq(innerClosure), block(
    assign(innerClosure, Closure(inner))
    ,
    CallClosure(
      closure = read(Reg.result, innerClosure),
      arguments = Seq(binOp(read(Reg.result, b), times, block(LIS(Reg.result), Word(encodeSigned(2))))),
      parameters = inner.parameters
    )))

    val middleClosure = new Variable("middleClosure")
    // Outer: create middle closure, call middle(a + 3)
    outer.code = Scope(Seq(middleClosure), block(
    assign(middleClosure, Closure(middle)),
    CallClosure(
      closure = read(Reg.result, middleClosure),
      arguments = Seq(binOp(read(Reg.result, a), plus, block(LIS(Reg.result), Word(encodeSigned(3))))),
      parameters = middle.parameters
    )
    ) )

    // Main: create outer closure, call outer(2)
    val main = new Procedure("main", Seq(dummy1, dummy2))
    val outerClosure = new Variable("outerClosure")
    main.code = Scope(Seq(outerClosure), block(
      assign(outerClosure, Closure(outer)),
      CallClosure(
        closure = read(Reg.result, outerClosure),
        arguments = Seq(block(LIS(Reg.result), Word(encodeSigned(2)))),
        parameters = outer.parameters
      )
    ))

    // Compile & run
    val machineCode = compileClosures(Seq(main, outer, middle, inner))
    val finalState = ProgramExamples.loadAndRun(
      machineCode,
      register1 = Word(encodeSigned(0)), // dummy1
      register2 = Word(encodeSigned(0)), // dummy2
      debug = true
    )

    // Computation trace:
    // outer(a=2) → middle(b=2+3=5) → inner(c=5*2=10) → inner returns c*2 = 20
    // middle returns 20, outer returns 20, main returns 20
    assert(decodeSigned(finalState.reg(3)) == 20)
    println("Complicated multi-level closure test result = " + decodeSigned(finalState.reg(3)))
  }

}
