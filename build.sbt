scalaVersion := "3.3.6"

name := "risc-assembly-compiler"

libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.19" % "test"

libraryDependencies += "com.lihaoyi" % "pprint_3" % "0.9.3"

Compile / scalaSource := baseDirectory.value / "src"

Test / scalaSource := baseDirectory.value / "test" / "scala"
