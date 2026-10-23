package standalone.runtime

import java.io.ByteArrayOutputStream

object CPU {
  val terminationPC: Word = Word("11111110111000011101111010101101")
  val maxAddr: Word = Word.fromLong(0x01000000L)
  val printAddr: Word = Word.fromLong(0xffff000cL)
  private var output = System.out

  def validAddress(address: Word): Boolean = {
    val n = Word.unsigned(address)
    n % 4 == 0 && n < Word.unsigned(maxAddr)
  }
  def checkValidAddress(address: Word): Unit =
    if (!validAddress(address)) sys.error(s"Invalid memory address $address")
  def checkValidPC(address: Word): Unit =
    if (address != terminationPC) checkValidAddress(address)

  private def read(state: State, register: Int): Long = Word.unsigned(state.reg(register))
  private def write(state: State, register: Int, value: Long): State =
    state.setReg(register, Word.fromLong(value))
  private def advance(state: State, words: Long): State = {
    val address = Word.fromLong(read(state, State.PC.toInt) + words * 4)
    checkValidPC(address)
    state.setReg(State.PC, address)
  }

  def step(initial: State): State = {
    val instruction = initial.mem(initial.reg(State.PC))
    val raw = Word.unsigned(instruction)
    val op = (raw >>> 26).toInt
    val source = ((raw >>> 21) & 31).toInt
    val target = ((raw >>> 16) & 31).toInt
    val destination = ((raw >>> 11) & 31).toInt
    val function = (raw & 63).toInt
    val immediate = raw.toShort.toLong
    val state = advance(initial, 1)
    val a = read(state, source)
    val b = read(state, target)
    val invalid = () => sys.error(s"Invalid instruction $instruction")
    op match {
      case 0 => function match {
        case 32 => write(state, destination, a + b)
        case 34 => write(state, destination, a - b)
        case 24 | 25 =>
          val product = if (function == 24) a.toInt.toLong * b.toInt.toLong else a * b
          write(write(state, State.LO.toInt, product), State.HI.toInt, product >>> 32)
        case 26 | 27 =>
          val (quotient, remainder) = if (function == 26)
            (a.toInt.toLong / b.toInt.toLong, a.toInt.toLong % b.toInt.toLong)
          else (a / b, a % b)
          write(write(state, State.LO.toInt, quotient), State.HI.toInt, remainder)
        case 16 => write(state, destination, read(state, State.HI.toInt))
        case 18 => write(state, destination, read(state, State.LO.toInt))
        case 20 => advance(state.setReg(destination, state.mem(state.reg(State.PC))), 1)
        case 42 => write(state, destination, if (a.toInt < b.toInt) 1 else 0)
        case 43 => write(state, destination, if (a < b) 1 else 0)
        case 8 | 9 =>
          val next = Word.fromLong(a)
          checkValidPC(next)
          val linked = if (function == 9) state.setReg(31, state.reg(State.PC)) else state
          linked.setReg(State.PC, next)
        case _ => invalid()
      }
      case 35 =>
        val address = Word.fromLong(a + immediate)
        checkValidAddress(address)
        state.setReg(target, state.mem(address))
      case 43 =>
        val address = Word.fromLong(a + immediate)
        if (address == printAddr) {
          output.print((b & 255).toChar)
          state
        } else {
          checkValidAddress(address)
          state.setMem(address, Word.fromLong(b))
        }
      case 4 => if (a == b) advance(state, immediate) else state
      case 5 => if (a != b) advance(state, immediate) else state
      case _ => invalid()
    }
  }

  def run(initial: State): State = {
    var state = initial
    while (state.reg(State.PC) != terminationPC) state = step(state)
    state
  }

  def captureOutput(body: => Unit): String = {
    val buffer = new ByteArrayOutputStream()
    val previous = output
    output = new java.io.PrintStream(buffer)
    try body finally output = previous
    buffer.toString
  }
}
