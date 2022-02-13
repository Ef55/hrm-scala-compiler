package hrsm

type Identifier = String
object Arguments {
  import Language.Value

  trait Argument
  case class Immediate(valueIndex: Value) extends Argument
  case class Indirect(addressIndex: Value) extends Argument
}

object Language {
  sealed trait Value {

    def +=(one: 1): Value = ???
    def -=(one: 1): Value = ???

    def +(v: Value): Value = ???

    override def toString: String = ???
  }

  sealed class ConcreteValue(val i: Int) extends Value {
    require((-999 to 999).contains(i))

    override def toString: String = i.toString
  }

  val uninitialized: Value = ???
  val inbox: Value = ???
  var outbox: Value = ???
}


object AST {
  type Value = Language.Value
  import Arguments.*

  sealed trait Tree
  case class Sequence(nodes: Seq[Tree]) extends Tree
  case class Define(id: Identifier, init: Option[Tree]) extends Tree
  case class Assign(id: Identifier, expr: Tree) extends Tree
  case object Inbox extends Tree
  case class Outbox(expr: Tree) extends Tree
  case class Variable(id: Identifier) extends Tree
  case class Add(lhs: Tree, rhs: Identifier) extends Tree
  case class Sub(lhs: Tree, rhs: Identifier) extends Tree
  case class BumpUp(id: Identifier) extends Tree
  case class BumpDown(id: Identifier) extends Tree
  case class Loop(body: Tree) extends Tree
}

object MachineCode {
  type Value = AST.Value
  import Arguments.*

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

  type Program = Seq[Instruction]
}