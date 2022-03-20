package hrsm

import MachineCode.*
import Language.{Argument, Immediate, Indirect}

import java.io.{File, FileWriter, BufferedWriter}

val identation = "    "

extension (arg: Argument) {
  def toText: String = arg match {
    case Immediate(value) => value.toString
    case Indirect(address) => s"[${address}]"
  }
}

extension (instr: Instruction) {
  def toText: String = instr match {
    case Inbox => s"${identation}INBOX"
    case Outbox => s"${identation}OUTBOX"
    case CopyTo(arg) => s"${identation}COPYTO ${arg.toText}"
    case CopyFrom(arg) => s"${identation}COPYFROM ${arg.toText}"
    case Add(arg) => s"${identation}ADD ${arg.toText}"
    case Sub(arg) => s"${identation}SUB ${arg.toText}"
    case BumpUp(arg) => s"${identation}BUMPUP ${arg.toText}"
    case BumpDown(arg) => s"${identation}BUMPDOWN ${arg.toText}"
    case Jump(id) => s"${identation}JUMP ${id}"
    case JumpZ(id) => s"${identation}JUMPZ ${id}"
    case JumpN(id) => s"${identation}JUMPN ${id}"
    case Label(id) => s"${id}:"
  }
}

def write(file: File, program: Program) =
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(program.pretty)
  bw.write("\n")
  bw.close()