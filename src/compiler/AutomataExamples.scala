package standalone.compiler

import standalone.syntax.DFAs.*

object AutomataExamples {

  val binaryNumbers = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "not0"),
    start = "start",
    accepting = Set("0", "not0"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "not0"
      case ("not0", _) => "not0"
    })


  lazy val notDiv3 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "rem0", "rem1", "rem2"),
    start = "start",
    accepting = Set("rem1", "rem2"),
    transition = {
      case ("start", '1') => "rem1"
      case ("rem1", '0') => "rem2"
      case ("rem1", '1') => "rem0"
      case ("rem0", '0') => "rem0"
      case ("rem0", '1') => "rem1"
      case ("rem2", '0') => "rem1"
      case ("rem2", '1') => "rem2"
    })


  lazy val notDiv23 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "rem0", "rem1Odd", "rem1Even", "rem2Odd", "rem2Even"), //rem is in terms of mod 3
    start = "start",
    accepting = Set("rem1Odd", "rem2Odd"),
    transition = {
      case ("start", '1') => "rem1Odd"
      case ("rem1Odd", '0') => "rem2Even"
      case ("rem1Odd", '1') => "rem0"
      case ("rem1Even", '0') => "rem2Even"
      case ("rem1Even", '1') => "rem0"
      case ("rem0", '0') => "rem0"
      case ("rem0", '1') => "rem1Odd"
      case ("rem2Odd", '0') => "rem1Even"
      case ("rem2Odd", '1') => "rem2Odd"
      case ("rem2Even", '0') => "rem1Even"
      case ("rem2Even", '1') => "rem2Odd"
    })


  lazy val decimalNumber = DFA(
    alphabet = "-0123456789".toSet,
    states = Set("start", "0", "-",
      "-1", "->=2", "-1 0or1", "-12", "-1 >=3", "->=2Any", "-128",
      "1", ">=2", "1 0or1", "12", "1 >=3", ">=2Any", "127",
    ),
    start = "start",
    accepting = Set(
      "0",
      "-1", "->=2", "-1 0or1", "-12", "-1 >=3", "->=2Any", "-128",
      "1", ">=2", "1 0or1", "12", "1 >=3", ">=2Any", "127"
    ),
    transition = {
      case ("start", '0') => "0"
      case ("start", '-') => "-"

      // After '-' we can only have digits, no double signs
      case ("-", d) if d.isDigit && d == '1' => "-1"
      case ("-", d) if d.isDigit && d >= '2' => "->=2"

      case ("-1", d) if d.isDigit && (d <= '1') => "-1 0or1"
      case ("-1", d) if d.isDigit && (d == '2') => "-12"
      case ("-1", d) if d.isDigit && (d >= '3') => "-1 >=3"

      case ("-1 0or1", d) if d.isDigit => "-128"
      case ("-12", d) if d.isDigit && (d <= '8') => "-128"

      case ("->=2", d) if d.isDigit => "->=2Any"

      case ("start", d) if d.isDigit && d == '1' => "1"
      case ("start", d) if d.isDigit && d >= '2' => ">=2"

      case ("1", d) if d.isDigit && (d <= '1') => "1 0or1"
      case ("1", d) if d.isDigit && (d == '2') => "12"
      case ("1", d) if d.isDigit && (d >= '3') => "1 >=3"

      case ("1 0or1", d) if d.isDigit => "127"
      case ("12", d) if d.isDigit && (d <= '7') => "127"

      case (">=2", d) if d.isDigit => ">=2Any"
    }
  )


  lazy val abc = DFA(
    alphabet = "abc".toSet,
    states = Set("start", "xax", "xaxbx", "xaxbxcx"),
    start = "start",
    accepting = Set("xaxbxcx"),
    transition = {
      case("start", 'a') => "xax"
      case ("start", d) if d != 'a' => "start"

      case("xax", 'b') => "xaxbx"
      case ("xax", d) if d != 'b' => "xax"

      case("xaxbx", 'c') => "xaxbxcx"
      case("xaxbx", d) if d != 'c' => "xaxbx"

      case ("xaxbxcx", _) => "xaxbxcx"
    }
  )


  lazy val abcSubstring = DFA(
    alphabet = "abc".toSet,
    states = Set("start", "xa", "xab", "xabcx"),
    start = "start",
    accepting = Set("xabcx"),
    transition = {
      case("start", 'a') => "xa"
      case("start", d) if d != 'a' => "start"

      case("xa", 'b') => "xab"
      case("xa", 'a') => "xa"
      case("xa", 'c') => "start"

      case("xab", 'c') => "xabcx"
      case("xab", 'b') => "start"
      case("xab", 'a') => "xa"

      case("xabcx", _) => "xabcx"
    }
  )
}
