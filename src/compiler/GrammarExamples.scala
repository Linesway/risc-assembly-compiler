package standalone.compiler

import standalone.syntax.DFAs.Token
import standalone.syntax.Grammars.*

object GrammarExamples {



  val grammar = parseGrammar(
    """
S BOF A EOF
A x
A A x
    """)


  val oneX: Tree =
    new Tree(Token("S"), Seq(
      new Tree(Token("BOF")),
      new Tree(Token("A"), Seq(
        new Tree(Token("x"))
      )),
      new Tree(Token("EOF"))
    ))


  lazy val twoXes: Tree =
    new Tree(Token("S"), Seq(
      new Tree(Token("BOF")),
      new Tree(Token("A"), Seq(
        new Tree(Token("A"), Seq(
          new Tree(Token("x"))
        )),
        new Tree(Token("x"))
      )),
      new Tree(Token("EOF"))
    ))


  lazy val threeXes: Tree =
    new Tree(Token("S"), Seq(
      new Tree(Token("BOF")),
      new Tree(Token("A"), Seq(
        new Tree(Token("A"), Seq(
          new Tree(Token("A"), Seq(
            new Tree(Token("x"))
          )),
          new Tree(Token("x"))
        )),
        new Tree(Token("x"))
      )),
      new Tree(Token("EOF"))
    ))


  val ambiguousGrammar = parseGrammar(
    """
S BOF A EOF
A x
A A x A
    """)




  lazy val tree1: Tree =
    new Tree(Token("S"), Seq(
      new Tree(Token("BOF")),
      new Tree(Token("A"), Seq(
        new Tree(Token("A"), Seq(
          new Tree(Token("A"), Seq(
            new Tree(Token("x"))
          )),
          new Tree(Token("x")),
          new Tree(Token("A"), Seq(
            new Tree(Token("x"))
          ))
        )),
        new Tree(Token("x")),
        new Tree(Token("A"), Seq(
          new Tree(Token("x"))
        ))
      )),
      new Tree(Token("EOF"))
    ))


  lazy val tree2: Tree =
    new Tree(Token("S"), Seq(
      new Tree(Token("BOF")),
      new Tree(Token("A"), Seq(
        new Tree(Token("A"), Seq(
          new Tree(Token("x"))
        )),
        new Tree(Token("x")),
        new Tree(Token("A"), Seq(
          new Tree(Token("A"), Seq(
            new Tree(Token("x"))
          )),
          new Tree(Token("x")),
          new Tree(Token("A"), Seq(
            new Tree(Token("x"))
          ))
        ))
      )),
      new Tree(Token("EOF"))
    ))
}
