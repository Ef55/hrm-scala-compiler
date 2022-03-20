import org.scalatest.*
import flatspec.*

import hrsm.*
import Language.{Immediate, Indirect, Literal}
import MachineCode.*

class Tests extends AnyFlatSpec {
  def chop(str: String) = str.stripMargin.drop(1)

  "Instructions" should "be printed correctly" in {
    val expected = chop("""
      |    INBOX
      |    OUTBOX
      |    COPYTO 0
      |    COPYFROM [1]
      |    ADD 2
      |    SUB [3]
      |    BUMPUP 4
      |    BUMPDOWN [5]
      |    JUMP a
      |    JUMPZ zero
      |    JUMPN labelwhichiswaytoolong
      |lbl:""")

    val program = Array(
      Inbox,
      Outbox,
      CopyTo(Immediate(Literal(0))),
      CopyFrom(Indirect(Literal(1))),
      Add(Immediate(Literal(2))),
      Sub(Indirect(Literal(3))),
      BumpUp(Immediate(Literal(4))),
      BumpDown(Indirect(Literal(5))),
      Jump("a"),
      JumpZ("zero"),
      JumpN("labelwhichiswaytoolong"),
      Label("lbl"),
    )

    assertResult(expected)(printProgram(program))
  }

}