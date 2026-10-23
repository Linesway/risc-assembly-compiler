package standalone.compiler

import standalone.runtime.*
import standalone.Utils.*

import scala.annotation.tailrec


object Assembler {






  def decodeUnsigned(bits: Seq[Boolean]): Long =
  {
    require(bits.length >= 0)
    require(bits.length <= 32)

    var finalNumber: Long = 0L
    for(i <- 0 to (bits.length - 1)) {
      if(bits(i))
        finalNumber += (1L << (bits.length - 1 - i))
    }

    finalNumber
  }


  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 1)
    require(bits.length <= 32)

    if(bits(0)) {
      val flippedSeq = bits.map(bit => !bit)
      -(decodeUnsigned(flippedSeq) + 1)
    } else {
      decodeUnsigned(bits)
    }
  }


  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 0)
    require(bits <= 32)
    require(i >= 0)
    require(i < twoTo(bits))

    (0 until bits).map { power =>
      ((1L << (bits - power - 1)) & i) != 0
    }
  }


  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 1)
    require(bits <= 32)
    require(i >= -twoTo(bits-1))
    require(i < twoTo(bits-1))

    if(i >= 0) {
      encodeUnsigned(i, bits)
    }
    else {
      //internally negative numbers are already in twos complement
      val twosComplement: Long = i & ((1L << bits) - 1) //keep lowest "bits" bits
      encodeUnsigned(twosComplement, bits)
    }
  }











  def ADD(d: Reg, s: Reg, t: Reg = Reg.zero): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeUnsigned(d.number, 5) ++ Bits("00000100000"))
  def SUB(d: Reg, s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeUnsigned(d.number, 5) ++ Bits("00000100010"))
  def MULT(s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ Bits("0000000000011000"))
  def MULTU(s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ Bits("0000000000011001"))
  def DIV(s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ Bits("0000000000011010"))
  def DIVU(s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ Bits("0000000000011011"))
  def MFHI(d: Reg): Word = Word(
    Bits("0000000000000000") ++ encodeUnsigned(d.number, 5) ++ Bits("00000010000"))
  def MFLO(d: Reg): Word = Word(
    Bits("0000000000000000") ++ encodeUnsigned(d.number, 5) ++ Bits("00000010010"))
  def LIS(d: Reg): Word = Word(
    Bits("0000000000000000") ++ encodeUnsigned(d.number, 5) ++ Bits("00000010100"))
  def LW(t: Reg, i: Int, s: Reg): Word = Word(
    Bits("100011") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeSigned(i, 16))
  def SW(t: Reg, i: Int, s: Reg): Word = Word(
    Bits("101011") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeSigned(i, 16))
  def SLT(d: Reg, s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeUnsigned(d.number, 5) ++ Bits("00000101010"))
  def SLTU(d: Reg, s: Reg, t: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeUnsigned(d.number, 5) ++ Bits("00000101011"))
  def BEQ(s: Reg, t: Reg, i: Int): Word = Word(
    Bits("000100") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeSigned(i, 16))
  def BNE(s: Reg, t: Reg, i: Int): Word = Word(
    Bits("000101") ++ encodeUnsigned(s.number, 5) ++ encodeUnsigned(t.number, 5) ++ encodeSigned(i, 16))
  def JR(s: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ Bits("000000000000000001000"))
  def JALR(s: Reg): Word = Word(
    Bits("000000") ++ encodeUnsigned(s.number, 5) ++ Bits("000000000000000001001"))
}
