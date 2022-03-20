package hrsm

import Language.*

import exproc.*

object hrprocessor extends exproc.Processor[Tree, Variable] {
  def assign[T](va: Variable[T], init: Tree[T]): Tree[Unit] = va match {
    case UserVariable(id) => Assign(id, init)
    case _ if va == outbox => Outbox(init.asInstanceOf[ArithTree])
    case _ => throw Exception(s"Unknown variable: ${va}")
  }

  def constant[T](t: T): Tree[T] = (t match {
    case i: Int => Literal(i)
    case true => True
    case () => Nop
    case _ => throw Exception(s"Unsupported constant: ${t}")
  }).asInstanceOf[Tree[T]]

  def ifThenElse[T](cond: Tree[Boolean], thenn: Tree[T], elze: Tree[T]): Tree[T] = cond match {
    case cnd: Cond => Ite(cnd, thenn, Some(elze))
    case True => thenn
    case _ => throw Exception(s"Unsupported condition: ${cond}")
  }

  def initialize[T](va: Variable[T], init: Tree[T]): Tree[Unit] = {
    val v = va.asInstanceOf[UserVariable[T]]
    init match {
      case Uninitialized => Define(v.id, None)
      case _ => Define(v.id, Some(init))
    }
  }

  def sequence[T](fsts: Seq[Tree[Any]], last: Tree[T]): Tree[T] = 
    Sequence(fsts, last)
  def variable[T](id: Identifier): Variable[T] = UserVariable(id)
  def whileLoop(cond: Tree[Boolean], body: Tree[Any]): Tree[Unit] = cond match {
    case cnd: Cond => ???
    case True => Loop(body)
    case _ => throw Exception(s"Unsupported condition: ${cond}")
  }
}
