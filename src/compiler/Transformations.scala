package standalone.compiler

import ProgramRepresentation.*
import CodeBuilders.*
import Assembler.*
import MemoryManagement.*
import standalone.runtime.*
import standalone.Utils.*
import Debugger.*

import scala.collection.mutable


object Transformations {









  def eliminateLabels(code: Seq[Code]): Seq[Word] = {
    val symbolTable = mutable.Map[Label, Int]()


    def setLabels(): Unit = {
      var address = 0
      code.foreach {
        case Define(label) =>
          if (symbolTable.contains(label))
            sys.error(s"Label ${label.name} defined multiple times")
          symbolTable(label) = address
        case _ => address += 1
      }
    }


    def translate: Seq[Word] = {
      var location = 0
      code.flatMap {
        case Define(_) => Nil
        case CodeWord(word) =>
          location += 1
          Seq[Word](word)
        case Use(label) =>
          location += 1
          Seq[Word](Word(encodeUnsigned(symbolTable(label) * 4L)))
        case BeqBne(bits, label) =>
          val pcLineRelativeOffset = symbolTable(label) - (location + 1)
          location += 1
          Seq[Word](Word(bits ++ encodeSigned(pcLineRelativeOffset, 16)))
        case other => impossible(s"Encountered unsupported code $other in eliminateLabels.")
      }
    }

    setLabels()
    translate
  }


  def link(codes1: Seq[Code], codes2: Seq[Code]): Seq[Code] = codes1 ++ codes2


  def eliminateComments(codes: Seq[Code]): Seq[Code] = {
    codes.flatMap {
      case Comment(_) => Nil
      case other => Seq[Code](other)
    }
  }


  def eliminateBlocks(code: Code): Seq[Code] = code match {
    case Block(children) => children.flatMap(child => eliminateBlocks(child))
    case _ => Seq(code) //termination condition
  }


  def transformCodeTotal(code: Code, fun: Code=>Code): Code = {

    def processChildren(code: Code): Code = code match {
      case Block(children) =>
        val newChildren = children.map(child => transformCodeTotal(child, fun))
        Block(newChildren)

      case Scope(variables, body) =>
        Scope(variables, transformCodeTotal(body, fun))

      case IfStmt(elseLabel, e1, comp, e2, thens, elses) =>
        IfStmt(elseLabel,
          transformCodeTotal(e1, fun),
          transformCodeTotal(comp, fun),
          transformCodeTotal(e2, fun),
          transformCodeTotal(thens, fun),
          transformCodeTotal(elses, fun))

      case Call(procedure, args, isTail) =>
        val newArgs = args.map(arg => transformCodeTotal(arg, fun))
        Call(procedure, newArgs, isTail)

      case CallClosure(closure, args, params, isTail) =>
        val newClosure = transformCodeTotal(closure, fun)
        val newArgs = args.map(arg => transformCodeTotal(arg, fun))
        CallClosure(newClosure, newArgs, params, isTail)

      case _ => code
    }

    fun(processChildren(code))
  }


  def transformCode(code: Code, fun: PartialFunction[Code, Code]): Code = {
    transformCodeTotal(code, code => if(fun.isDefinedAt(code)) fun(code) else code)
  }






  def eliminateFrameAccesses(code: Code, frame: Chunk): Code = {
    def fun: PartialFunction[Code, Code] = {
      case va: VarAccess =>
        if(va.read) {
          frame.load(Reg.framePointer, va.register, va.variable)
        }
        else {
          frame.store(Reg.framePointer, va.variable, va.register)
        }
    }

    transformCode(code, fun)
  }


  def allocateFrameOnStack(body: Code, frame: Chunk): Code =
    block(
      block(
        LIS(Reg.scratch),
        Word(encodeUnsigned(frame.bytes)),
        SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
        ADD(Reg.framePointer, Reg.stackPointer, Reg.zero)
      ),
      body,
      block(
        LIS(Reg.scratch),
        Word(encodeUnsigned(frame.bytes)),
        ADD(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      )
    )


  case class MachineCode(words: Seq[Word], debugTable: DebugTable)


  def toMachineCode(code: Code): MachineCode = {
    val code2 = eliminateBlocks(code)
    val code3 = eliminateComments(code2)
    val code4 = eliminateLabels(code3)
    val debugTable = createDebugTable(code2)
    MachineCode(code4, debugTable)
  }


  def compileWithFrame(code: Code, variables: Seq[Variable]): MachineCode = {
    val frame = Chunk(variables)
    val code1 = eliminateFrameAccesses(code, frame)
    val code2 = allocateFrameOnStack(code1, frame)
    toMachineCode(block(code2, JR(Reg.link)))
  }






  def eliminateScopes(code: Code): (Code, Seq[Variable]) = {
    var tempVars = Seq[Variable]()
    def fun: PartialFunction[Code, Code] = {
      case scope: Scope =>
        tempVars ++= scope.variables
        scope.code
    }
    val resultCode = transformCode(code, fun)
    (resultCode, tempVars)
  }


  def eliminateIfStmts(code: Code): Code = {
    def fun: PartialFunction[Code, Code] = {
      case ifStatement: IfStmt =>
        val temp = new Variable("temp", false)
        val endLabel = new Label("endLabel")
        val elseLabel = ifStatement.elseLabel
        Scope(Seq(temp), block(
          Comment("Begin if statement"),
          ifStatement.e1,
          Comment(s"Condition value 1 is now in Reg.result (above)"),
          write(temp, Reg.result),
          ifStatement.e2,
          Comment(s"Condition value 2 is now in Reg.result (above)"),
          read(Reg.scratch, temp),
          ifStatement.comp,
          ifStatement.thens,
          beq(Reg.zero, Reg.zero, endLabel),
          Define(elseLabel),
          ifStatement.elses,
          Define(endLabel)
        ))
    }

    transformCode(code, fun)
  }

  def compileStructured(code: Code): MachineCode = {
    val code1 = eliminateIfStmts(code)
    val (code2, variables) = eliminateScopes(code1)
    assert(variables.distinct == variables)
    compileWithFrame(code2, variables)
  }






  def createTempVars(params: Seq[Variable]): Seq[Variable] =
    params.map(param => new Variable("temp", param.isPointer))


  def makeMap[A, B](keys: Seq[A], function: A=>B): Map[A, B] = keys.map(key => (key, function(key))).toMap


  def compileProcedures(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures


    val paramChunks: Map[Procedure, Chunk] =
      makeMap(procedures, procedure => Chunk(procedure.parameters))


    def compileProcedure(currentProcedure: Procedure): Code = {


      def eliminateCalls(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case call: Call =>
            // Note: `caller` does not need to be used in the solution for `eliminateCalls`.
            // It is made explicit here only to distinguish it from `callee`.
            val caller = currentProcedure
            val callee = call.procedure

            val tempVars = createTempVars(callee.parameters)

            val argsToTempVars: Seq[Code] = tempVars.indices.map(i =>
              block(
                call.arguments(i),
                write(tempVars(i), Reg.result)
              ))

            val allocatedParamChunk = new Variable("allocatedParamChunk", false)

            val tempVarsToChunk: Seq[Code] = tempVars.indices.map(i =>
              block(
                read(Reg.result, allocatedParamChunk),
                read(Reg.scratch, tempVars(i)),
                paramChunks(callee).store(Reg.result, paramChunks(callee).variables(i), Reg.scratch)
              )
            )

            block(
              Scope(tempVars ++ Seq(allocatedParamChunk), block(
                Block(argsToTempVars),

                Stack.allocate(paramChunks(callee)), //allocate param chunk onto stack and store chunk address in Reg.result
                write(allocatedParamChunk, Reg.result),

                Block(tempVarsToChunk),
                read(Reg.result, allocatedParamChunk),

                LIS(Reg.targetPC),
                Use(callee.label),
                JALR(Reg.targetPC)
              ))
            )
        }

        transformCode(code, fun)
      }


      val code1 = eliminateCalls(currentProcedure.code)
      val code2 = eliminateIfStmts(code1)
      val (code3, variables) = eliminateScopes(code2)
      assert(variables.distinct == variables)

      val frameVariables: Seq[Variable] = Seq(currentProcedure.paramPtr, currentProcedure.dynamicLink, currentProcedure.savedPC)
      val frame = Chunk(frameVariables ++ variables)


      def addEntryExit(code: Code): Code = {

        val enter = block(
          Comment(s"for current procedure, start enter procedureName: $currentProcedure"),
          // Step 1: Save parameter chunk address (we'll store it in frame later)
          ADD(Reg.savedParamPtr, Reg.zero, Reg.result),

          Stack.allocate(frame), //allocate space for the procedure frame on stack

          frame.store(Reg.result, currentProcedure.dynamicLink, Reg.framePointer),// save frame pointer to dynamic link
          ADD(Reg.framePointer, Reg.result, Reg.zero), //set frame pointer to the address of the frame allocation
          frame.store(Reg.framePointer, currentProcedure.savedPC, Reg.link), //save Reg.link to savedPC

          Comment(s"for current procedure: $currentProcedure, save parameter chunk address to paramPtr"),

          frame.store(Reg.framePointer, currentProcedure.paramPtr, Reg.savedParamPtr), //save parameter chunk address to paramPtr
        )
        val exit = block(
          frame.load(Reg.framePointer, Reg.link, currentProcedure.savedPC), //restore Reg.link
          frame.load(Reg.framePointer, Reg.framePointer, currentProcedure.dynamicLink), //restore Reg.framePointer

          Stack.pop,
          Stack.pop,

          JR(Reg.link) //return control to caller
        )
        block(Define(currentProcedure.label), enter, code, exit)
      }


      def eliminateProcedureAccesses(code: Code): Code = {

        def readVar(v: Variable, reg: Reg): Code = {
          if (frame.variables.contains(v)) {
            // local variable
            frame.load(Reg.framePointer, reg, v)
          } else {
            // parameter variable
            block(
              Comment(s"reading parameter: $v, $currentProcedure"),
              frame.load(Reg.framePointer, Reg.scratch, currentProcedure.paramPtr), // get param chunk
              paramChunks(currentProcedure).load(Reg.scratch, reg, v) // read param
            )
          }
        }
        def writeVar(v: Variable, reg: Reg): Code = {
          if (frame.variables.contains(v)) {
            frame.store(Reg.framePointer, v, reg)
          } else {
            block(
              frame.load(Reg.framePointer, Reg.scratch, currentProcedure.paramPtr), // get param chunk
              paramChunks(currentProcedure).store(Reg.scratch, v, reg) // write param
            )
          }
        }

        def fun: PartialFunction[Code, Code] = {
          case va: VarAccess =>
            if (va.read) {
              readVar(va.variable, va.register)
            }
            else {
              writeVar(va.variable, va.register)
            }
        }

        transformCode(code, fun)
      }


      val code4 = eliminateProcedureAccesses(code3)
      addEntryExit(code4)
    }



    val code = block(
      Stack.allocate(Chunk(procedures.head.parameters)), // allocate parameter chunk for start procedure
      Block(procedures.map(compileProcedure))
    )
    toMachineCode(code)
  }

  def startProcedure(mainProcedure: Procedure): Procedure = {
    val ret = new Procedure("start", Seq())
    ret.code = Call(mainProcedure, Seq(ADD(Reg.result, Reg.input1), ADD(Reg.result, Reg.input2)))
    ret
  }





  val closureCode = new Variable("closure code", isPointer = true)
  val closureEnvironment = new Variable("closure environment", isPointer = true)

  lazy val closureChunk = Chunk(Seq(closureCode, closureEnvironment))


  def compileClosures(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures


    val paramChunks: Map[Procedure, Chunk] = makeMap(procedures, procedure => {
        Chunk(procedure.parameters :+ procedure.staticLink)
      }
    )


    val frameOnHeap: Set[Procedure] = {
      var frameOnHeap = Set[Procedure]()
      for proc <- procedures do
        def fun: PartialFunction[Code, Code] = {
          case closure: Closure =>
            var p: Option[Procedure] = Some(closure.procedure)
            while p != null && p.isDefined && !frameOnHeap(p.get) do
              frameOnHeap += p.get
              p = p.get.outer
            closure
        }
        transformCode(proc.code, fun)
      frameOnHeap
    }


    def phaseOne(currentProcedure: Procedure): (Code, Chunk) = {


      def computeStaticLink(callee: Procedure): Code = {
        val caller = currentProcedure


        val n = caller.depth - callee.depth + 1

        if(callee.outer.isEmpty)
        {
          block(
            Comment(s"computeStaticLink: there is no outer enclosing procedure"),
            ADD(Reg.result, Reg.zero)
          )
        }
        else if(n == 0)
        {
          block(
            Comment(s"computeStaticLink: n = 0; pass the frame pointer"),
            ADD(Reg.result, Reg.framePointer)
          )
        }
        else
        {
          var proc = Option(caller)
          for (_ <- 0 until (n-1)) { // only need to follow n - 1 times, since we will access staticLink an extra time into Reg.result
            proc = proc.get.outer
          }

          block(
            Comment(s"computeStaticLink: reading static link $n-1 times"),
            read(Reg.result, proc.get.staticLink) // come back to this if it doesn't work, since we are assuming staticLink is defined here
          )
        }
      }


      def eliminateCalls(code: Code): Code = {
        val caller = currentProcedure

        def fun: PartialFunction[Code, Code] = {
          case call: Call =>
            val callee = call.procedure
            val tempVars = createTempVars(paramChunks(callee).variables)
            if call.isTail then
              Scope(tempVars, block(
                Block(tempVars.zip(call.arguments :+ computeStaticLink(callee)).map(assign)),
                (if (frameOnHeap(caller)) heap else Stack).allocate(paramChunks(callee)),
                Block(tempVars.zip(paramChunks(callee).variables).flatMap((t, v) => Seq(
                  read(Reg.scratch, t),
                  paramChunks(callee).store(Reg.result, v, Reg.scratch),
                ))),
                ADD(Reg.allocParamsScratch, Reg.result),
                read(Reg.link, caller.savedPC),
                read(Reg.framePointer, caller.dynamicLink),
                if (frameOnHeap(caller)) block() else block(Stack.pop, Stack.pop, Stack.pop),
                (if (frameOnHeap(callee)) heap else Stack).allocate(paramChunks(callee)),
                Comment(s"param chunk of $callee"),
                copyChunk(Reg.result, Reg.allocParamsScratch),
                LIS(Reg.targetPC),
                Use(callee.label),
                Comment(s"HERE WE GO FROM $caller TO $callee"),
                JR(Reg.targetPC),
              ))
            else
              Scope(tempVars, block(
                Block(tempVars.zip(call.arguments :+ computeStaticLink(callee)).map(assign)),
                (if (frameOnHeap(callee)) heap else Stack).allocate(paramChunks(callee)),
                Comment(s"param chunk of $callee"),
                Block(tempVars.zip(paramChunks(callee).variables).flatMap((t, v) => Seq(
                  read(Reg.scratch, t),
                  paramChunks(callee).store(Reg.result, v, Reg.scratch),
                ))),
                LIS(Reg.targetPC),
                Use(callee.label),
                JALR(Reg.targetPC),
              ))
          case callClosure: CallClosure =>
            val chunkAddr = Variable("chunkAddr", true)
            val newClosureChunk = Chunk(callClosure.parameters :+ Variable("closure staticLink", true))
            val tempVars = createTempVars(newClosureChunk.variables)
            if callClosure.isTail then
              Scope(chunkAddr +: tempVars, block(
                Comment("Closure Call Start"),
                assign(chunkAddr, callClosure.closure),
                Block(tempVars.zip(callClosure.arguments :+ block(
                  read(Reg.scratch, chunkAddr),
                  closureChunk.load(Reg.scratch, Reg.result, closureEnvironment),
                )).map(assign)),
                heap.allocate(newClosureChunk),
                Comment(s"Param Chunk"),
                Block(tempVars.zip(newClosureChunk.variables).flatMap((t, v) => Seq(
                  read(Reg.scratch, t),
                  newClosureChunk.store(Reg.result, v, Reg.scratch),
                ))),
                read(Reg.scratch, chunkAddr),
                closureChunk.load(Reg.scratch, Reg.targetPC, closureCode),
                read(Reg.link, caller.savedPC),
                read(Reg.framePointer, caller.dynamicLink),
                if (frameOnHeap(caller)) block() else block(Stack.pop, Stack.pop),
                Comment(s"Executing JR $caller"),
                JR(Reg.targetPC),
              ))
            else
              Scope(chunkAddr +: tempVars, block(
                Comment("Start Call Closure (not tail)"),
                assign(chunkAddr, callClosure.closure),
                Block(tempVars.zip(callClosure.arguments :+ block(
                  Comment("Start Loading Closure Environment"),
                  read(Reg.scratch, chunkAddr),
                  closureChunk.load(Reg.scratch, Reg.result, closureEnvironment),
                  Comment("Finish Loading Closure Environment"),
                )).map(assign)),
                heap.allocate(newClosureChunk),
                Comment(s"Param Chunk is now in Reg.result"),
                Block(tempVars.zip(newClosureChunk.variables).flatMap((t, v) => Seq(
                  read(Reg.scratch, t),
                  newClosureChunk.store(Reg.result, v, Reg.scratch),
                ))),
                read(Reg.scratch, chunkAddr),
                closureChunk.load(Reg.scratch, Reg.targetPC, closureCode),
                JALR(Reg.targetPC),
                Comment("Returned from Closure Call"),
              ))
        }

        transformCode(code, fun)
      }


      def eliminateClosures(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case closure: Closure =>
            val closureStaticLink = new Variable("closureStaticLink", true)
            Scope(Seq(closureStaticLink), block(
              assign(closureStaticLink, computeStaticLink(closure.procedure)),
              heap.allocate(closureChunk),

              LIS(Reg.scratch),
              Use(closure.procedure.label),
              closureChunk.store(Reg.result, closureCode, Reg.scratch),

              read(Reg.scratch, closureStaticLink),
              closureChunk.store(Reg.result, closureEnvironment, Reg.scratch)
            ))
        }

        transformCode(code, fun)
      }


      def detectTailCalls(code: Code): Code = {

        code


//        def nested(procedure: Procedure): Boolean = {
//          procedure == currentProcedure || (procedure.outer != null && procedure.outer.exists(nested))
//        }
//
//        def mapLast(codes: Seq[Code], fun: Code=>Code): Seq[Code] = {
//          def isInstruction(code: Code): Boolean = {
//            code match {
//              case Comment(message) => false
//              case Define(label) => false
//              case Scope(variables, code) => isInstruction(code)
//              case Block(stmts) => stmts.exists(isInstruction)
//              case _ => true
//            }
//          }
//          def loop(reved: Seq[Code], acc: Seq[Code]): Seq[Code] = {
//            if reved.isEmpty then
//              acc
//            else if isInstruction(reved.head) then
//              reved.tail.reverse ++ (fun(reved.head) +: acc)
//            else
//              loop(reved.tail, reved.head +: acc)
//          }
//          loop(codes.reverse, Seq())
//        }
//
//        def tail(code: Code): Code = code match {
//          case Block(children) => Block(mapLast(children, tail))
//          case Scope(variables, body) => Scope(variables, tail(body))
//          case IfStmt(elseLabel, e1, comp, e2, thens, elses) =>
//            IfStmt(elseLabel, e1, comp, e2, tail(thens), tail(elses))
//          case Call(procedure, args, isTail) => Call(procedure, args, procedure == currentProcedure || !nested(procedure))
//          case _ => code
//        }
//
//        tail(code)
      }



      val code1 = eliminateClosures(currentProcedure.code)
      val code2 = detectTailCalls(code1)
      val code3 = eliminateCalls(code2)
      val code4 = eliminateIfStmts(code3)
      val (code5, variables) = eliminateScopes(code4)
      assert(variables.distinct == variables)

      val frameVariables: Seq[Variable] = Seq(currentProcedure.paramPtr, currentProcedure.dynamicLink, currentProcedure.savedPC) // copied from ProcedureExamples
      val frame = Chunk(frameVariables ++ variables)

      (code5, frame)
    }

    val phaseOneResults: Map[Procedure, (Code, Chunk)] = makeMap(procedures, phaseOne)

    def phaseTwo(currentProcedure: Procedure): Code = {
      val (code, frame) = phaseOneResults(currentProcedure)


      def addEntryExit(code: Code): Code = {
        val callee = currentProcedure
        val enter = block(
          Comment(s"start entering $callee"),
          ADD(Reg.savedParamPtr, Reg.result),
          //          (if (frameOnHeap(callee)) heap else Stack).allocate(frame),
          Stack.allocate(frame),
          frame.store(Reg.result, callee.dynamicLink, Reg.framePointer),
          ADD(Reg.framePointer, Reg.result),
          frame.store(Reg.framePointer, callee.savedPC, Reg.link),
          frame.store(Reg.framePointer, callee.paramPtr, Reg.savedParamPtr),
          if frameOnHeap(callee) then
            block(
              heap.allocate(frame),
              copyChunk(Reg.result, Reg.framePointer),
              ADD(Reg.framePointer, Reg.result),
              Stack.pop,
            )
          else Comment(s"no heap copy"),
          Comment(s"finish entering $callee"),
        )
        val exit = block(
          Comment(s"start exiting $callee"),
          frame.load(Reg.framePointer, Reg.link, callee.savedPC),
          frame.load(Reg.framePointer, Reg.framePointer, callee.dynamicLink),
          if (frameOnHeap(callee)) block() else block(Stack.pop, Stack.pop),
          Comment(s"finish exiting $callee"),
          JR(Reg.link),
        )
        block(Define(callee.label), enter, code, exit)
      }


      def eliminateVarAccesses(code: Code): Code = {
        def fun: PartialFunction[Code, Code] = {
          case va: VarAccess =>
            def recurse(proc: Procedure, code: Seq[Code]): Seq[Code] = {

              val currentFrame = phaseOneResults(proc)._2

              if (currentFrame.variables.contains(va.variable)) {
                // local variable
                if(va.read) {
                  code :+ currentFrame.load(Reg.scratch, va.register, va.variable)
                }
                else {
                  code :+ currentFrame.store(Reg.scratch, va.variable, va.register)
                }
              } else if (paramChunks(proc).variables.contains(va.variable)) {
                // parameter variable
                code :+ block(
                  Comment(s"parameter: $va, $proc"),
                  currentFrame.load(Reg.scratch, Reg.scratch, proc.paramPtr), // get param chunk
                  if(va.read) {
                    paramChunks(proc).load(Reg.scratch, va.register, va.variable) // read param
                  } else {
                    paramChunks(proc).store(Reg.scratch, va.variable, va.register) // write param
                  }
                )
              }
              else {
                recurse(proc.outer.get,
                  code :+ block(
                    Comment(s"Walking up the staticLinks now, checking for $va"),
                    currentFrame.load(Reg.scratch, Reg.scratch, proc.paramPtr), // load paramPtr into Reg.scratch
                    paramChunks(proc).load(Reg.scratch, Reg.scratch, proc.staticLink) // load staticLink into Reg.scratch from paramChunks(proc) and paramPtr
                  )
                )
              }
            }
            Block(recurse(currentProcedure, Seq(ADD(Reg.scratch, Reg.framePointer)))) //reg.scratch will now contain the current framepointer
        }

        transformCode(code, fun)
      }



      val code1 = eliminateVarAccesses(code)
      val code2 = addEntryExit(code1)
      code2
    }



    val code = block(
      heap.initCode,
      Stack.allocate(Chunk(procedures.head.parameters :+ procedures.head.staticLink)), // allocate parameter chunk for start procedure
      Block(procedures.map(phaseTwo))
    )
    toMachineCode(code)
  }

}
