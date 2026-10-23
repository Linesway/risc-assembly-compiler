package standalone.compiler


object Reg {

  val link = Reg(31)

  val stackPointer = Reg(30)

  val framePointer = Reg(29)

  val heapPointer = Reg(28)

  val fromSpaceEnd = Reg(27)


  val input1 = Reg(1)
  val input2 = Reg(2)

  val result = Reg(3)

  val scratch = Reg(4)

  val savedParamPtr = Reg(5)

  val copyChunkScratch = Reg(7)

  val targetPC = Reg(8)

  val scratchPtrForGC = Reg(9)

  val copyChunkScratch2 = Reg(10)
  val allocParamsScratch = Reg(11) //holds the chunk address of param chunk after allocating param chunk in eliminateCalls
  val allocClosureChunk = Reg(12) //holds the chunk address of closure chunk after allocating closure chunk in eliminateCalls

  val scratch2 = Reg(15)
  val scratch3 = Reg(16)
  val scratch4 = Reg(17)

  val oldBlockAddr = Reg(14)


  val zero = Reg(0)
}

case class Reg(number: Int) {
  require(number >= 0)
  require(number <= 31)
}
