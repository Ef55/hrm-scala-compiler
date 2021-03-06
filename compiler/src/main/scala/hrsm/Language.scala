package hrsm

type Identifier = Int

object Language {
  trait Argument
  case class Immediate(valueIndex: Literal) extends Argument
  case class Indirect(addressIndex: Literal) extends Argument

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
  case object Nop extends Tree[Unit]
  case class Sequence[+T](nodes: Seq[Tree[Any]], last: Tree[T]) extends Tree[T]
  case class Define[+T](id: Identifier, init: Option[Tree[T]]) extends Tree[Unit]
  case class Assign[+T](id: Identifier, expr: Tree[T]) extends Tree[Unit]
  sealed trait Variable[+T] extends Tree[T]
  case class UserVariable[+T](id: Identifier) extends Variable[T]
  case class Loop(body: Tree[Any]) extends Tree[Nothing]
  case class Ite[+T](cond: Cond, thenn: Tree[T], elze: Option[Tree[T]]) extends Tree[T]

  type ArithTree = Tree[Int]
  case class Add(lhs: ArithTree, rhs: Identifier) extends ArithTree
  case class Sub(lhs: ArithTree, rhs: Identifier) extends ArithTree
  case class BumpUp(id: Identifier) extends ArithTree
  case class BumpDown(id: Identifier) extends ArithTree

  case object Inbox extends ArithTree
  case object OutboxVar extends Variable[Int]
  case class Outbox(v: ArithTree) extends Tree[Unit]
  case object Uninitialized extends ArithTree
  case class Literal(i: Int) extends ArithTree {
    require((-999 to 999).contains(i), s"Literal must be between -999 and 999; was ${i}.")

    override def toString = i.toString
  }

  case class Cond(comparee: ArithTree, comparator: Comparator) extends Tree[Boolean]
  case object True extends Tree[Boolean]

  
}

object MachineCode {
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
  case class Jump(identifier: String) extends Instruction
  case class JumpZ(identifier: String) extends Instruction
  case class JumpN(identifier: String) extends Instruction
  case class Label(identifier: String) extends Instruction

  type Program = Seq[Instruction]

  extension (program: Program)
    def pretty: String = program.map(_.toText).mkString("\n")

}