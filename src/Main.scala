package standalone

import java.nio.file.{Files, Path}
import standalone.compiler.{Lacs, MachineRunner}
import standalone.runtime.{CPU, Word}

object Main {
  def main(args: Array[String]): Unit = args.toList match {
    case "compile" :: input :: output :: Nil =>
      val code = Lacs.compile(Files.readString(Path.of(input)))
      val bytes = code.words.flatMap { word =>
        val value = Word.unsigned(word)
        (3 to 0 by -1).map(shift => (value >>> (shift * 8)).toByte)
      }.toArray
      Files.write(Path.of(output), bytes)
      println(s"Wrote ${code.words.size} machine words to $output")
    case "run" :: input :: first :: second :: Nil =>
      val code = Lacs.compile(Files.readString(Path.of(input)))
      val state = MachineRunner.setMem(code.words)
        .setReg(1, Word.fromLong(first.toLong))
        .setReg(2, Word.fromLong(second.toLong))
      val result = CPU.run(state)
      println(Word.signed(result.reg(3)))
    case _ =>
      sys.error("Usage: runMain standalone.Main compile <source.lacs> <output.bin> | runMain standalone.Main run <source.lacs> <arg1> <arg2>")
  }
}
