package hrsm

type Identifier = exproc.Identifier

object Language {
  trait Argument
  case class Immediate(valueIndex: Value) extends Argument
  case class Indirect(addressIndex: Value) extends Argument

  enum Comparator:
    case Eq
    case Neq
    case Less
    case Geq

  extension (cmp: Comparator)
    def invert: Comparator = cmp match 
      case Comparator.Eq => Comparator.Neq
      case Comparator.Neq => Comparator.Eq
      case Comparator.Less => Comparator.Geq
      case Comparator.Geq => Comparator.Less

  sealed trait Tree[+T]
  case class Sequence[+T](nodes: Seq[Tree[Any]], last: Tree[T]) extends Tree[T]
  case class Define[+T](id: Identifier, init: Option[Tree[T]]) extends Tree[Unit]
  case class Assign[+T](id: Identifier, expr: Tree[T]) extends Tree[Unit]
  case class Variable[+T](id: Identifier) extends Tree[T]
  case class Loop(body: Tree[Any]) extends Tree[Nothing]
  case class Ite[+T](cond: Cond, thenn: Tree[T], elze: Option[Tree[T]]) extends Tree[T]

  sealed trait ArithTree extends Tree[Int]
  case class Add(lhs: ArithTree, rhs: Identifier) extends ArithTree
  case class Sub(lhs: ArithTree, rhs: Identifier) extends ArithTree
  case class BumpUp(id: Identifier) extends ArithTree
  case class BumpDown(id: Identifier) extends ArithTree

  sealed trait Value extends ArithTree
  case object Inbox extends Value
  case object Outbox extends Value 
  case object Uninitialized extends Value
  case class Literal(i: Int) extends Value {
    require((-999 to 999).contains(i), s"Literal must be between -999 and 999; was ${i}.")
  }

  case class Cond(comparee: ArithTree, comparator: Comparator) extends Tree[Boolean]
  case object True extends Tree[Boolean]


  extension (lhs: ArithTree) {
    def +(rhs: Identifier): ArithTree = Add(lhs, rhs)
    def -(rhs: Identifier): ArithTree = Sub(lhs, rhs)

    def ===(zero: 0): Cond = Cond(lhs, Comparator.Eq)
    def !==(zero: 0): Cond = Cond(lhs, Comparator.Neq)
    def <(zero: 0): Cond = Cond(lhs, Comparator.Less)
    def >=(zero: 0): Cond = Cond(lhs, Comparator.Geq)

    // def ==(v: Value): Boolean = ???
    // def !=(v: Value): Boolean = ???
    // def <(v: Value): Boolean = ???
    // def >=(v: Value): Boolean = ???
  }

  extension (lhs: Variable[Int]) {
    def +=(one: 1): ArithTree = BumpUp(lhs.id)
    def -=(one: 1): ArithTree = BumpDown(lhs.id)
  }

  val uninitialized: Value = Uninitialized
  val inbox: Value = Inbox
  var outbox: Value = Outbox
}

object MachineCode {
  type Value = Language.Value
  import Language.Argument

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