import standalone.compiler.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.MemoryManagement.*
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, State, Word}
import standalone.syntax.DFAs.Token
import standalone.syntax.Grammars
import standalone.syntax.Grammars.Tree
import org.scalatest.funsuite.AnyFunSuite

class GarbageCollectorTests extends AnyFunSuite {
  test("lacs") {
    def compileAndRun(prog: String, a: Int = 0, b: Int = 0, debug: Boolean = false) = {
      val machineCode = Lacs.compileWithGarbageCollector(prog)
      val finalState = ProgramExamples.loadAndRun(machineCode, Word(encodeSigned(a)), Word(encodeSigned(b)), debug=debug)
      println(decodeSigned(finalState.reg(3)))
    }

    compileAndRun("def main(a: Int, b: Int): Int = { a + b }", 4, 7, debug = false)
    compileAndRun("def main(a: Int, b: Int): Int = { a * b }", 4, 7, debug = false)
    val prog =
      """
      def main(a: Int, b: Int): Int = {
        increaseBy(a)(b)
      }
      def increaseBy(increment: Int): (Int)=>Int = {
        def procedure(x: Int): Int = { x + increment }
        procedure
      }
        """
    compileAndRun(prog, 4, 7, debug = false)
    // GC runs only if
    // - program uses closures
    // - from-space is full
    // For testing, modify alocateProc to call collectGarbage on EVERY allocation
  }
  test("heaps") {
    def v(q: Variable) = {
      read(Reg.result, q)
    }

    withGC {
      val a = new Variable("a")
      val b = new Variable("b")
      val main = new Procedure("main", Seq(a,b))
      val f = new Variable("f", isPointer = true)
      val chunk = Chunk(Seq(f))
      def writeField(base: Code, field: Variable, value: Code) = {
        binOp(base, chunk.store(Reg.scratch, field, Reg.result), value)
      }

      val p = new Variable("p", isPointer = true)
      val q = new Variable("q", isPointer = true)
      main.code = Scope(Seq(p,q), block(
        assign(p, heap.allocate(chunk)),
        assign(q, heap.allocate(chunk)),
        writeField(v(q), f, v(p)),
        writeField(v(p), f, v(q)),
        assign(p, v(q)),
        call(GarbageCollector.collectGarbage),
        ADD(Reg.zero, Reg.zero, Reg.zero) // prevent tail call
      ))
      val machineCode = compileClosures(Seq(main) ++ GarbageCollector.procedures)
      val finalState = ProgramExamples.loadAndRun(machineCode)
      def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
        if(words>0) {
          println(spaces(address) + ": " + spaces(state.mem(address)))
          dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address)+4)), words-1)
        }
      }
      def spaces(w: Word): String = w.toString().sliding(8,8).mkString(" ")

      println("Semispace 1:")
      dumpMem(finalState, GarbageCollector.heapStart)
      println("Semispace 2:")
      dumpMem(finalState, GarbageCollector.heapMiddle)
      println("heapPtr: " + spaces(finalState.reg(Reg.heapPointer.number)))
      println("reachable bytes: " + decodeSigned(finalState.reg(Reg.result.number)))

    }
  }
}
