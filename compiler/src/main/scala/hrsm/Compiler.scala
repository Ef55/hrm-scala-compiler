package hrsm

type Identifier = String
type Value = Int

trait Argument
case class Immediate(value: Value) extends Argument
case class Indirect(address: Value) extends Argument

trait Instruction
case object Inbox extends Instruction
case object Outbox extends Instruction
case class CopyTo(argument: Argument) extends Instruction
case class CopyFrom(argument: Argument) extends Instruction
case class Add(argument: Argument) extends Instruction
case class Sub(argument: Argument) extends Instruction
case class BumpUp(argument: Argument) extends Instruction
case class BumpDown(argument: Argument) extends Instruction
case class Jump(identifier: Identifier) extends Instruction
case class JumpZ(identifier: Identifier) extends Instruction
case class JumpN(identifier: Identifier) extends Instruction
case class Label(identifier: Identifier) extends Instruction
