package standalone.compiler

import standalone.compiler.Assembler.*
import standalone.compiler.CodeBuilders.*
import standalone.compiler.ProgramRepresentation.*
import standalone.runtime.*


object MemoryManagement {


  case class Chunk(variables: Seq[Variable]) {

    private val sortedVariables: Seq[Variable] = {
      val (pointers, nonPointers) = variables.partition(_.isPointer)
      pointers ++ nonPointers
    }


    val words: Int = 2 + variables.size

    val bytes: Int = words * 4


    private val variableToOffset: Map[Variable, Int] = {
      sortedVariables.view.zipWithIndex.map {
        case (variable, index) =>
          val offset = (index + 2) * 4
          (variable, offset)
      }.toMap
    }


    def load(baseRegister: Reg, register: Reg, variable: Variable): Code = {
      block(
        Comment(s"loading variable: $variable"),
        LW(register, variableToOffset(variable), baseRegister)
      )
    }



    def store(baseRegister: Reg, variable: Variable, register: Reg): Code = {
      SW(register, variableToOffset(variable), baseRegister)
    }


    def initialize: Code = {
      val entry   = Label("Init:ChunkScanBegin")
      val exit    = Label("Init:ChunkScanExit")

      // precompute some constants
      val totalSize    = encodeUnsigned(bytes)
      val ptrCount     = encodeUnsigned(variables.count(_.isPointer))
      val fourBytes    = encodeUnsigned(4)

      block(
        // store the chunk byte size
        LIS(Reg.scratch), Word(totalSize),
        SW(Reg.scratch, 0, Reg.result),

        // compute the upper bound address in scratch2
        ADD(Reg.scratch2, Reg.result, Reg.scratch),

        // prepare constant 4 in scratch3
        LIS(Reg.scratch3), Word(fourBytes),

        // address where pointer-count goes
        ADD(Reg.scratch, Reg.result, Reg.scratch3),

        // write pointer-count
        LIS(Reg.scratch4), Word(ptrCount),
        SW(Reg.scratch4, 0, Reg.scratch),

        // jump into the loop
        beq(Reg.zero, Reg.zero, entry),

        // loop exit point
        Define(exit),
        SW(Reg.zero, 0, Reg.scratch),

        // beginning of loop body
        Define(entry),

        // move to next chunk in memory
        ADD(Reg.scratch, Reg.scratch, Reg.scratch3),

        // terminate when we reach the limit
        bne(Reg.scratch, Reg.scratch2, exit)
      )
    }
  }


  abstract class MemoryAllocator {

    val initCode: Code = block()


    def allocate(chunk: Chunk): Code
  }


  object Stack extends MemoryAllocator {

    def allocate(chunk: Chunk): Code = block(
      Comment("alloc start"),
      LIS(Reg.result),
      Word(encodeUnsigned(chunk.bytes)),
      SUB(Reg.stackPointer, Reg.stackPointer, Reg.result),
      ADD(Reg.result, Reg.stackPointer),
      Comment("alloc end"),
      chunk.initialize,
    )


    val pop: Code = block(
      Comment("pop start"),
      LW(Reg.scratch, 0, Reg.stackPointer),
      ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      Comment("pop end"),
    )
  }


  def copyChunk(toRegister: Reg, fromRegister: Reg): Code = {

    val modifiedRegisters = Set(Reg.scratch, Reg.copyChunkScratch, Reg.copyChunkScratch2)
    require(!modifiedRegisters.contains(toRegister))
    require(!modifiedRegisters.contains(fromRegister))

    val whileLoopLabel = new Label("whileLoopLabel")
    block(
      LW(Reg.scratch, 0, fromRegister),
      ADD(Reg.scratch, Reg.scratch, fromRegister),
      Define(whileLoopLabel),
      LW(Reg.copyChunkScratch, -4, Reg.scratch),
      SUB(Reg.scratch, Reg.scratch, fromRegister),
      ADD(Reg.scratch, Reg.scratch, toRegister),
      SW(Reg.copyChunkScratch, -4, Reg.scratch),
      ADD(Reg.scratch, Reg.scratch, fromRegister),
      SUB(Reg.scratch, Reg.scratch, toRegister),
      LIS(Reg.copyChunkScratch),
      Word(encodeUnsigned(4)),
      SUB(Reg.scratch, Reg.scratch, Reg.copyChunkScratch),
      bne(Reg.scratch, fromRegister, whileLoopLabel),
    )
  }

  private val heapImplementation = new scala.util.DynamicVariable[MemoryAllocator](SimpleHeapAllocator)

  def heap: MemoryAllocator = heapImplementation.value

  trait HeapSettings {

    val memSize = decodeUnsigned(CPU.maxAddr)

    val heapStart = Word(encodeUnsigned(memSize / 4))

    val heapMiddle = Word(encodeUnsigned(memSize / 2))

    val heapEnd = Word(encodeUnsigned(memSize * 3 / 4))
  }


  private object SimpleHeapAllocator extends MemoryAllocator with HeapSettings {

    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart)


    def allocate(chunk: Chunk): Code = block(
      ADD(Reg.result, Reg.heapPointer),
      chunk.initialize,
      LIS(Reg.scratch),
      Word(encodeUnsigned(chunk.bytes)),
      ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch),
    )
  }





  object GarbageCollector extends MemoryAllocator with HeapSettings {


    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart, LIS(Reg.fromSpaceEnd), heapMiddle)



    val allocateProc_bytes = new Variable("allocateProc_bytes")
    val allocateProc = new Procedure("allocateProc", Seq(allocateProc_bytes))

    val collectGarbage = new Procedure("collectGarbage", Seq())

    val block_forwardPtrs = new Variable("block_forwardPtrs")
    val forwardPtrs = new Procedure("forwardPtrs", Seq(block_forwardPtrs), Some(collectGarbage))

    val block_copy = new Variable("block_copy")
    val copy = new Procedure("copy", Seq(block_copy), Some(collectGarbage))


    def procedures: Seq[Procedure] = Seq(allocateProc, collectGarbage, forwardPtrs, copy)


    allocateProc.code = {
      // create a temporary variable to save the old heap pointer
      val savedHeapPtr = new Variable("savedHeapPtr")

      // generate the main scope for allocation
      Scope(Seq(savedHeapPtr), block(
        Comment("Start of allocateProc"),

        // check if heap needs to grow
        ifStmt(
          binOp(ADD(Reg.result, Reg.heapPointer), plus, read(Reg.result, allocateProc_bytes)),
          gtCmp,
          block(
            ADD(Reg.result, Reg.fromSpaceEnd)
          ),
          block(
            call(collectGarbage)
          )
        ),

        // save the current heap pointer
        assign(savedHeapPtr, ADD(Reg.result, Reg.heapPointer)),

        // compute new heap pointer
        read(Reg.scratch, allocateProc_bytes),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch),

        // return old heap pointer as result
        read(Reg.result, savedHeapPtr)
      ))
    }


    def allocate(chunk: Chunk): Code = {
      // create a chunk for the procedure parameters + static link
      val procChunk = Chunk(allocateProc.parameters :+ allocateProc.staticLink)

      // encode the chunk bytes into a register
      val encodedBytes = Word(encodeUnsigned(chunk.bytes))

      // build the sequence of instructions
      val codeSequence = block(
        Stack.allocate(procChunk),
        LIS(Reg.scratch),
        encodedBytes,
        procChunk.store(Reg.result, allocateProc_bytes, Reg.scratch),
        LIS(Reg.targetPC),
        Use(allocateProc.label),
        JALR(Reg.targetPC),
        chunk.initialize
      )

      codeSequence
    }


    val fromSpaceStart = new Variable("fromSpaceStart")
    val free = new Variable("free")
    collectGarbage.code = {
      val newToSpace = new Variable("newToSpace")
      val scanPtr = new Variable("scanPtr")

      Scope(Seq(newToSpace, fromSpaceStart, free, scanPtr),
        block(
          Comment("Start of garbage collection"),

          Comment("Determine to-space and from-space boundaries"),
          ifStmt(
            ADD(Reg.result, Reg.fromSpaceEnd),
            eqCmp,
            block(LIS(Reg.result), heapMiddle),
            block(
              assign(newToSpace, block(LIS(Reg.result), heapMiddle)),
              assign(fromSpaceStart, block(LIS(Reg.result), heapStart))
            ),
            block(
              assign(newToSpace, block(LIS(Reg.result), heapStart)),
              assign(fromSpaceStart, block(LIS(Reg.result), heapMiddle))
            )
          ),

          Comment("Initialize free pointer"),
          assign(free, read(Reg.result, newToSpace)),

          Comment("Initialize scan pointer from dynamic link"),
          assign(scanPtr, read(Reg.result, collectGarbage.dynamicLink)),

          Comment("Scan all stack frames until maximum address"),
          whileLoop(
            read(Reg.result, scanPtr),
            ltCmp,
            block(LIS(Reg.result), CPU.maxAddr),
            block(
              call(forwardPtrs, binOp(read(Reg.result, scanPtr), plus, block(LIS(Reg.result), Word(encodeSigned(4L))))),
              assign(scanPtr, binOp(read(Reg.result, scanPtr), plus, deref(read(Reg.result, scanPtr))))
            )
          ),

          Comment("Reset scan pointer to start of to-space"),
          assign(scanPtr, read(Reg.result, newToSpace)),

          Comment("Scan all blocks in to-space until free pointer"),
          whileLoop(
            read(Reg.result, scanPtr),
            ltCmp,
            read(Reg.result, free),
            block(
              call(forwardPtrs, binOp(read(Reg.result, scanPtr), plus, block(LIS(Reg.result), Word(encodeSigned(4L))))),
              assign(scanPtr, binOp(read(Reg.result, scanPtr), plus, deref(read(Reg.result, scanPtr))))
            )
          ),

          Comment("Update from-space end based on to-space placement"),
          ifStmt(
            ADD(Reg.result, Reg.fromSpaceEnd),
            eqCmp,
            block(LIS(Reg.result), heapMiddle),
            block(LIS(Reg.fromSpaceEnd), heapEnd),
            block(LIS(Reg.fromSpaceEnd), heapMiddle)
          ),

          Comment("Update heap pointer to free pointer"),
          read(Reg.heapPointer, free),

          Comment("Return number of live bytes"),
          binOp(ADD(Reg.result, Reg.heapPointer), minus, read(Reg.result, newToSpace))
        )
      )
    }


    // ------------------ forwardPtrs ------------------
    forwardPtrs.code = {
      val lastPtrAddr = new Variable("lastPtrAddr")
      val newPointerAddr = new Variable("newPointerAddr")

      Scope(Seq(lastPtrAddr, newPointerAddr), block(
        Comment("Forward pointer setup"),
        assign(lastPtrAddr, binOp(
          read(Reg.result, block_forwardPtrs),
          plus,
          binOp(deref(read(Reg.result, block_forwardPtrs)), times, block(LIS(Reg.result), Word(encodeSigned(4L))))
        )),

        Comment("Move block_forwardPtrs to start of pointer variables"),
        assign(block_forwardPtrs, binOp(read(Reg.result, block_forwardPtrs), plus, block(LIS(Reg.result), Word(encodeSigned(4L))))),

        Comment("Loop through all pointer variables"),
        whileLoop(
          read(Reg.result, block_forwardPtrs),
          leCmp,
          read(Reg.result, lastPtrAddr),
          block(
            assign(newPointerAddr, call(copy, deref(read(Reg.result, block_forwardPtrs)))),
            assignToAddr(read(Reg.result, block_forwardPtrs), read(Reg.result, newPointerAddr)),
            assign(block_forwardPtrs, binOp(read(Reg.result, block_forwardPtrs), plus, block(LIS(Reg.result), Word(encodeSigned(4L)))))
          )
        )
      ))
    }

    // ------------------ copy ------------------
    copy.code = {
      val blockSizeVar = new Variable("blockSize")

      Scope(Seq(blockSizeVar), block(
        Comment("Copy block from from-space to to-space"),
        ifStmt(
          read(Reg.result, block_copy),
          geCmp,
          ADD(Reg.result, Reg.fromSpaceEnd),
          block(Comment("Block already beyond from-space"), read(Reg.result, block_copy)),
          ifStmt(
            read(Reg.result, block_copy),
            ltCmp,
            read(Reg.result, fromSpaceStart),
            block(Comment("Block before from-space"), read(Reg.result, block_copy)),
            block(
              assign(blockSizeVar, deref(read(Reg.result, block_copy))),
              ifStmt(
                read(Reg.result, blockSizeVar),
                geCmp,
                ADD(Reg.result, Reg.zero),
                block(
                  Comment("Block not yet copied"),
                  read(Reg.result, free),
                  read(Reg.oldBlockAddr, block_copy),
                  copyChunk(Reg.result, Reg.oldBlockAddr),
                  assignToAddr(binOp(read(Reg.result, block_copy), plus, block(LIS(Reg.result), Word(encodeSigned(4L)))), read(Reg.result, free)),
                  assignToAddr(read(Reg.result, block_copy), binOp(ADD(Reg.result, Reg.zero), minus, read(Reg.result, blockSizeVar))),
                  assign(free, binOp(read(Reg.result, free), plus, deref(read(Reg.result, free))))
                ),
                Comment("Block already copied")
              ),
              deref(binOp(read(Reg.result, block_copy), plus, block(LIS(Reg.result), Word(encodeSigned(4L)))))
            )
          )
        )
      ))
    }
  }



  def withGC[T](code: => T): T = withHeap(GarbageCollector, code)

  def withHeap[T](newHeap: MemoryAllocator, code: => T): T =
    heapImplementation.withValue(newHeap)(code)
}
