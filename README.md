# MIPS32 Compiler

[![CI](https://github.com/Linesway/mips32-compiler/actions/workflows/ci.yml/badge.svg)](https://github.com/Linesway/mips32-compiler/actions/workflows/ci.yml)

A Scala 3 compiler that turns a small typed language with nested procedures and closures into 32-bit MIPS machine code. It scans and parses source code, checks types, and generates executable instructions. The source language is called Lacs; the repository also includes a MIPS interpreter, assembler, debugger, and garbage collector.

## Requirements

- Java 17 or newer
- [sbt](https://www.scala-sbt.org/download/) (the project pins sbt 1.11.4 and Scala 3.3.6)

Run the test suite with `sbt test`.

## Usage

To compile a program to a binary file:

```sh
sbt "runMain standalone.Main compile examples/add.lacs add.bin"
```

To compile and execute a program locally:

```sh
sbt "runMain standalone.Main run examples/add.lacs 4 7"
```

The example returns `11`. A Lacs `main` function takes two integer arguments; its result is read from MIPS register 3. The binary file contains big-endian 32-bit instruction words.

The compiler uses local runtime and parser types, with no bundled JAR. The code is available under the [MIT license](LICENSE).
