import standalone.compiler.*
import standalone.compiler.ProcedureExamples.*
import standalone.compiler.AutomataExamples.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.Lacs.*
import standalone.compiler.MemoryManagement.Chunk
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Scanning.*
import standalone.compiler.Transformations.*
import standalone.compiler.Typer.*
import standalone.runtime.{CPU, Word}
import standalone.syntax.DFAs.*
import standalone.syntax.Grammars.*
import org.scalatest.funsuite.AnyFunSuite
import standalone.compiler.*
import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.MemoryManagement.Chunk
import standalone.compiler.ProgramRepresentation.*
import standalone.compiler.Transformations.*
import standalone.runtime.{CPU, Word}
import standalone.syntax.DFAs.Token
import standalone.syntax.Grammars
import standalone.syntax.Grammars.Tree
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable


class CompilationTests extends AnyFunSuite {
  test("test") {
    val prog =
      """
      def main(a: Int, b: Int): Int = {a+b}
      def foo(c: Int, d: (Int,Int)=>Int ): Int = { d(c,c) }
        """
    val tree = Lacs.scanAndParse(prog)
    val typedProcedures = Typer.typeTree(tree)
    println(typedProcedures)
    val procedures = CodeGenerator.generateProcedures(typedProcedures = typedProcedures)
    println(procedures)
    //disassemblingCodePrinter.pprintln(procedures(0).code)
    val machineCode = compileClosures(procedures)
    val finalState = ProgramExamples.loadAndRun(machineCode, Word(encodeSigned(3)), Word(encodeSigned(5)))
    println(decodeSigned(finalState.reg(3)))
  }
  test("lacs") {
    def compileAndRun(prog: String, a: Int = 0, b: Int = 0, debug: Boolean = false) = {
      val machineCode = Lacs.compile(prog)
      val finalState = ProgramExamples.loadAndRun(machineCode, Word(encodeSigned(a)), Word(encodeSigned(b)), debug)
      println(decodeSigned(finalState.reg(3)))
    }

    compileAndRun("def main(a: Int, b: Int): Int = {a+b}", 4, 7, debug = true)
    compileAndRun("def main(a: Int, b: Int): Int = {a*b}", 4, 7)
  }

  test("triple nested closures") {
    val prog =
      """
       def main(a: Int, b: Int): Int = {
         var x: Int;
         var y: Int;
         var z: Int;

         def outer(): Int = {
           def middle(): Int = {
             def inner(): Int = {
               x + y + z
             }
             inner() + 1
           }
           middle() + 2
         }

         x = a;
         y = b;
         z = a + b;

         outer() + 3
       }
       """
    val machineCode = Lacs.compile(prog)
    val finalState = ProgramExamples.loadAndRun(machineCode, Word(encodeSigned(2)), Word(encodeSigned(3)))
    println(decodeSigned(finalState.reg(3))) // expect 16
  }

  test("nested if-else in closures") {
    val prog =
      """
        def main(a: Int, b: Int): Int = {
            var v: Int;
            v = outer()(a) + b;
            v
        }

         def outer(): (Int) => Int = {
            def f(x: Int): Int = {
                var v: Int;
                v = 1;
                v + x
            }
            f
        }

       """
    val machineCode = Lacs.compile(prog)
    val finalState = ProgramExamples.loadAndRun(machineCode, Word(encodeSigned(4)), Word(encodeSigned(7)))
    println(decodeSigned(finalState.reg(3))) //expects 12 correct
  }

  test("extreme brackets and closures") {
    val prog =
      """
      def main(a: Int, b: Int): Int = {
        var x: Int;
        var y: Int;
        var z: Int;

        def f(u: Int): Int = {
          (((((u + x))))) + (((((y)))))
        }

        def g(h: (Int) => Int): Int = {
          (((h((((z))))))) + ((((x))))   // g applies h with many brackets
        }

        x = (((((a))))) ;
        y = (((b + (a)))) * (((1 + (2)))) ;
        z = (((((a + b))))) ;

        (((g((((f)))))))   // call g(f) with absurd parentheses
      }
      """

    // a=3, b=4:
    // x = 3
    // y = (4 + 3) * (1 + 2) = 7 * 3 = 21
    // z = 7
    //
    // f(u) = (u + x) + y
    //      = (u + 3) + 21
    //
    // g(h) = h(z) + x
    //       = h(7) + 3
    //
    // h = f:
    //
    // f(7) = (7 + 3) + 21 = 31
    //
    // result = 31 + 3 = 34

    val machineCode = Lacs.compile(prog)
    val finalState = ProgramExamples.loadAndRun(machineCode, Word(encodeSigned(3)), Word(encodeSigned(4)))
    println(decodeSigned(finalState.reg(3))) // expect 34
  }
}
