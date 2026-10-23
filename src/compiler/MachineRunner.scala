package standalone.compiler

import standalone.compiler.Assembler.*
import standalone.runtime.*

object MachineRunner {



  def setMem(words: Seq[Word], inputState: State = State(), startingAddress: Word = Word.zero): State = {
    require(decodeUnsigned(startingAddress) + words.size*4 <= decodeUnsigned(CPU.maxAddr))

    var resultState = inputState

    for(i: Int <- words.indices) {
      val addr = encodeUnsigned(decodeUnsigned(startingAddress) + i*4)
      resultState = resultState.setMem(Word(addr), words(i))
    }
    resultState
  }


  def loadAndRun(words: Seq[Word], register1: Word = Word.zero, register2: Word = Word.zero): State = {
    val initialState =
      setMem(words)
        .setReg(1, register1)
        .setReg(2, register2)
    CPU.run(initialState)
  }



  //Jump Register jr $s 0000 00ss sss0 0000 0000 0000 0000 1000
  lazy val transferTo31 = Seq[Word](Word("00000011111000000000000000001000"))



  //Load Word lw $t, i($s) 1000 11ss ssst tttt iiii iiii iiii iiii I $t = MEM [$s + i]
  //Store Word sw $t, i($s) 1010 11ss ssst tttt iiii iiii iiii iiii I MEM [$s + i] = $
  //Add add $d, $s, $t 0000 00ss ssst tttt dddd d000 0010 0000 R $d = $s + $t

  lazy val add134 = Seq[Word](
    Word("00000000001000000001100000100000"),
    Word("00000000001000110010000000100000"),
    Word("00000011111000000000000000001000"),
    JR(Reg(31))
  )




  lazy val maximum = Seq[Word](
    SLT(Reg(4), Reg(1), Reg(2)), //1 if Reg(1) < Reg(2), 0 otherwise
    BEQ(Reg(4), Reg(0), 2), //skip two lines if Reg(1) > Reg(2)
    //we know that Reg(2) is the max
    ADD(Reg(3), Reg(2), Reg(0)),
    JR(Reg(31)),
    //we know that Reg(1) is the max
    ADD(Reg(3), Reg(1), Reg(0)),
    JR(Reg(31))
  )


  lazy val addOne = Seq[Word](
    LIS(Reg(2)),
    Word("00000000000000000000000000000001"),
    ADD(Reg(3), Reg(1), Reg(2)),
    JR(Reg(31))
  )


  lazy val followingAddress = Seq[Word](
    LIS(Reg(2)),
    Word("00000000000000000000000000000100"),
    ADD(Reg(3), Reg(1), Reg(2)),
    JR(Reg(31))
  )
}
