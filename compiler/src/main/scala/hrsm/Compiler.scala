package hrsm

import scala.collection.mutable.Map
import scala.quoted.*

type Identifier = String


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

given ValueToExpr: ToExpr[ConcreteValue] with {
  def apply(v: ConcreteValue)(using Quotes): Expr[ConcreteValue] =
    '{ConcreteValue(${summon[ToExpr[Int]].apply(v.i)})}
}

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

type Program = Seq[Instruction]

inline def hrassembly(inline expr: Any): Program = ${hraImpl('expr)}


object builtins {
  val uninitialized: Value = ???
  val inbox: Value = ???
  var outbox: Value = ???
}

def hraImpl(expr: Expr[Any])(using Quotes): Expr[Program] =
  import quotes.reflect.*

  var lastLabel = -1
  def freshLabel: Expr[Identifier] =
    lastLabel += 1
    Expr(s"l${lastLabel}")

  val variables = Map.empty[String, ConcreteValue]
  var lastVar = -1
  def defineVariable(name: String): Expr[ConcreteValue] =
    lastVar += 1
    variables += (name -> ConcreteValue(lastVar))
    getVar(name)

  def getVar(name: String): Expr[ConcreteValue] = Expr(variables(name))

  object transform extends TreeMap {
    override def transformStats(ls: List[Statement])(owner: Symbol): List[Statement] = 
      ls.map {
        case t: Term => transformTerm(t)(owner)

        case ValDef(name, tt, Some(init)) => 
          if(tt.tpe =:= TypeRepr.of[Value]) then
            val loc = defineVariable(name)
            '{
              val initInstr = ${transformTerm(init)(owner).asExprOf[Program]}
              if initInstr.isEmpty then initInstr
              else initInstr :+ CopyTo(Immediate(${loc}))
            }.asTerm
          else throw RuntimeException(s"Unsupported type: ${tt.tpe.show} (expected: ${TypeRepr.of[Value].show})")

        case s => throw RuntimeException(s"Unsupported statement: ${s.getClass}")
      }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      case Literal(UnitConstant()) => Expr(Nil).asTerm

      case Ident("inbox") => '{List[Instruction](Inbox)}.asTerm
      case Ident("outbox") => throw RuntimeException("Cannot access outbox value")
      case Ident("uninitialized") => Expr(Nil).asTerm
      case Ident(varName) => '{List[Instruction](CopyFrom(Immediate(${getVar(varName)})))}.asTerm

      case Assign(Ident("outbox"), expr) => 
        '{
          ${transformTerm(expr)(owner).asExprOf[Program]} :+ Outbox
        }.asTerm
      case Assign(Ident(varName), expr) =>
        val loc = getVar(varName)
        '{
          ${transformTerm(expr)(owner).asExprOf[Program]} :+ CopyTo(Immediate(${loc}))
        }.asTerm


      case Apply(Select(Ident(varName), methodName), List(Literal(IntConstant(1)))) => {
        methodName match 
          case "+=" => '{List[Instruction](BumpUp(Immediate(${getVar(varName)})))}
          case "-=" => '{List[Instruction](BumpDown(Immediate(${getVar(varName)})))}
          case _ => throw RuntimeException(s"Unsupported operation on Value: ${methodName}")
        }.asTerm

      case Apply(Select(lhs, methodName), List(Ident(rhsName))) => {
        methodName match 
          case "+" => '{ ${transformTerm(lhs)(owner).asExprOf[Program]} :+ Add(Immediate(${getVar(rhsName)})) }
          case "-" => '{ ${transformTerm(lhs)(owner).asExprOf[Program]} :+ Sub(Immediate(${getVar(rhsName)})) }
          case _ => throw RuntimeException(s"Unsupported operation on Value: ${methodName}")
        }.asTerm

      case Block(statements, expr) => 
        val tStats = '{
          ${Expr.ofList(transformStats(statements)(owner).map(_.asExprOf[Program]))}.flatten
        }
        val tExpr = transformTerm(expr)(owner).asExprOf[Program]
        '{
          ${tStats} ++ 
          ${tExpr}
        }.asTerm

      case While(Literal(BooleanConstant(true)), body) => 
        val tBody = transformTerm(body)(owner)
        val label = freshLabel
        '{ 
          (Label($label) +: ${tBody.asExprOf[Program]}) :+ Jump($label) 
        }.asTerm

      case Inlined(_, _, code) => 
        transformTerm(code)(owner)

      case _ => throw RuntimeException(s"Unsupported construct: ${Printer.TreeStructure.show(t)}")
  }

  transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[Program]
