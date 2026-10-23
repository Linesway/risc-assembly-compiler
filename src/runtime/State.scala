package standalone.runtime

final case class State private (
    private val registers: Vector[Word],
    private val memory: Map[Long, Word]
) {
  def reg(number: Long): Word = registers(number.toInt)
  def mem(address: Word): Word = memory.getOrElse(Word.unsigned(address), Word.zero)
  def setReg(number: Long, word: Word): State =
    if (number == 0) this else copy(registers = registers.updated(number.toInt, word))
  def setMem(address: Word, word: Word): State =
    copy(memory = memory.updated(Word.unsigned(address), word))
}

object State {
  val PC = 32L
  val LO = 33L
  val HI = 34L
  def apply(): State =
    new State(Vector.fill(35)(Word.zero), Map.empty)
      .setReg(31, CPU.terminationPC)
      .setReg(30, CPU.maxAddr)
}
