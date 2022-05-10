import org.scalatest.*
import flatspec.*

import hrsm.*

import scala.io.Source

class Tests extends AnyFlatSpec {
  def chop(str: String) = str.stripMargin.drop(1)

  "Instructions" should "be printed correctly" in {
    import Language.{Tree, Immediate, Indirect, Literal}
    import MachineCode.*
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

    assertResult(expected)(program.pretty)
  }

  import Language.{*, given}
  import MachineCode.Program

  val expectedPath = "compiler/src/test/scala/expected"

  inline def yearTest(year: Year): Unit = {
    val result = year.code.pretty

    val expected = Source.fromFile(s"${expectedPath}/year${year.name}.asm")
      .getLines.mkString("\n")

    assertResult(expected)(result)
  }

  "Year 2" should "yield expected code" in {
    yearTest(year2)
  }

  "Year 6" should "yield expected code" in {
    yearTest(year6)
  }

  "Year 9" should "yield expected code" in {
    yearTest(year9)
  }

  "Year 12" should "yield expected code" in {
    yearTest(year12)
  }

  // "Year 17" should "yield expected code" in {
  //   yearTest(year17)
  // }

  // "Year 28" should "yield expected code" in {
  //   yearTest(year28)
  // }

}