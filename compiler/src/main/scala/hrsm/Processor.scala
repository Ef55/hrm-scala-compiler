package hrsm

import Language.*

import exproc.{ Identifier as _, * }

import scala.compiletime.error

object hrprocessor extends exproc.AstBuilder[Tree] {
  type Variable[T] = Language.Variable[T]

  private def getVarId(v: Variable[Int]): Identifier = (v: Variable[Int]) match {
    case UserVariable(id) => id
    case OutboxVar => throw new RuntimeException("Invalid use of outbox")
  }
  
  extension (lhs: ArithTree) {
    def +(rhs: Variable[Int]): ArithTree = Add(lhs, getVarId(rhs))
    def -(rhs: Variable[Int]): ArithTree = Sub(lhs, getVarId(rhs))

    def ===(zero: 0): Cond = Cond(lhs, Comparator.Eq)
    def !==(zero: 0): Cond = Cond(lhs, Comparator.Neq)
    def <(zero: 0): Cond = Cond(lhs, Comparator.Less)
    def >=(zero: 0): Cond = Cond(lhs, Comparator.Geq)

    def ===(rhs: Variable[Int]): Cond = Sub(lhs, getVarId(rhs)) === 0
    def !==(rhs: Variable[Int]): Cond = Sub(lhs, getVarId(rhs)) !== 0
    def <(rhs: Variable[Int]): Cond = Sub(lhs, getVarId(rhs)) < 0
    def >=(rhs: Variable[Int]): Cond = Sub(lhs, getVarId(rhs)) >= 0
  }

  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T])(inline elze: Tree[T]) =
    hrprocessor.ifThenElse(cond, thenn, elze)

  inline def If[T](inline cond: Tree[Boolean])(inline thenn: Tree[T]) =
    hrprocessor.ifThenElse(cond, thenn, Nop)

  inline def While(inline cond: Tree[Boolean])(inline body: Tree[?]) =
    hrprocessor.whileLoop(cond, body)

  given autoConst[T]: Conversion[T, Tree[T]] with 
    def apply(t: T) = constant(t)

  extension (lhs: Variable[Int]) {
    def +=(one: 1): ArithTree = BumpUp(getVarId(lhs))
    def -=(one: 1): ArithTree = BumpDown(getVarId(lhs))
  }

  val uninitialized: ArithTree = Uninitialized
  val inbox: ArithTree = Inbox
  var outbox: Variable[Int] = OutboxVar


  private var varCount = 0
  private def freshId: Identifier = {
    val id = varCount
    varCount += 1
    id
  }
  override inline def freshVariable[T]: Variable[T] = UserVariable[T](freshId): Variable[T]

  override inline def initialize[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit] = {
    val v = va.asInstanceOf[UserVariable[T]]
    init match {
      case Uninitialized => Define(v.id, None)
      case _ => Define(v.id, Some(init))
    }
  }

  override inline def assign[T](inline va: Variable[T], inline init: Tree[T]): Tree[Unit] = (va: Variable[T]) match {
    case UserVariable(id) => Assign(id, init)
    case OutboxVar => Outbox(init.asInstanceOf[ArithTree])
  }

  override inline def constant[T](inline t: T): Tree[T] = (t match {
    case i: Int => Literal(i)
    case true => True
    case () => Nop
    case _ => throw new RuntimeException("Unsupported constant: " + scala.compiletime.codeOf(t))
  }).asInstanceOf[Tree[T]]

  override inline def combine[T, S](inline l: Tree[T], inline r: Tree[S]): Tree[S] = (l, r) match 
    case (Sequence(sl, ll), Sequence(sr, lr)) => Sequence((sl :+ ll) ++ sr, lr)
    case (Sequence(sl, ll), r) => Sequence(sl :+ ll, r)
    case (l, Sequence(sr, lr)) => Sequence(l +: sr, lr)
    case (l, r) => Sequence(Seq(l), r)


  def ifThenElse[T](cond: Tree[Boolean], thenn: Tree[T], elze: Tree[T]): Tree[T] = cond match {
    case cnd: Cond => Ite(cnd, thenn, Some(elze))
    case True => thenn
    case _ => throw new RuntimeException(s"Unsupported condition: ${cond}")
  }

  def whileLoop(cond: Tree[Boolean], body: Tree[Any]): Tree[Unit] = cond match {
    case cnd: Cond => ???
    case True => Loop(body)
    case _ => throw new RuntimeException(s"Unsupported condition: ${cond}")
  }


}
