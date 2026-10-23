package standalone

object Utils {
  def twoTo(power: Int): Long = 1L << power
  def mod2to32(number: Long): Long = number & 0xffffffffL
  def mod2to32signed(number: Long): Long = mod2to32(number).toInt.toLong
  def impossible(reason: String = ""): Nothing = sys.error(s"Impossible state: $reason")
  extension [A](items: Seq[A]) {
    def splitAt(lengths: List[Int]): List[Seq[A]] = lengths match {
      case Nil => List(items)
      case first :: rest =>
        val (head, tail) = items.splitAt(first)
        head :: tail.splitAt(rest)
    }
  }
}
