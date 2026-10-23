package standalone.runtime

object Bits {
  def apply(text: String): Seq[Boolean] = text.filterNot(_ == ' ').map {
    case '0' => false
    case '1' => true
    case other => throw new IllegalArgumentException(s"Invalid bit: $other")
  }

  def unapply(bits: Seq[Boolean]): Option[String] =
    Some(bits.map(if (_) '1' else '0').mkString)
}

final class Word private (private val value: Vector[Boolean]) extends IndexedSeq[Boolean] {
  override def length: Int = 32
  override def apply(index: Int): Boolean = value(index)
  override def equals(other: Any): Boolean = other match {
    case word: Word => value == word.value
    case _ => false
  }
  override def hashCode(): Int = value.hashCode()
  override def toString: String = value.map(if (_) '1' else '0').mkString
}

object Word {
  def apply(bits: Seq[Boolean]): Word = {
    require(bits.size == 32)
    new Word(bits.toVector)
  }
  def apply(bits: String): Word = Word(Bits(bits))
  val zero: Word = Word(Vector.fill(32)(false))
  def fromLong(value: Long): Word = Word((31 to 0 by -1).map(bit => ((value >>> bit) & 1L) == 1L))
  def unsigned(word: Seq[Boolean]): Long = word.foldLeft(0L)((n, bit) => (n << 1) | (if (bit) 1L else 0L))
  def signed(word: Seq[Boolean]): Long = {
    val value = unsigned(word)
    if (word.headOption.contains(true)) value - (1L << word.size) else value
  }
}
