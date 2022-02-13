package hrsm

import Arguments.*
import Language.*

import scala.collection.mutable.Map
import scala.quoted.*

given ValueToExpr: ToExpr[ConcreteValue] with {
  def apply(v: ConcreteValue)(using Quotes): Expr[ConcreteValue] =
    '{ConcreteValue(${summon[ToExpr[Int]].apply(v.i)})}
}

inline def hrprocessor(inline expr: Any): AST.Tree = ${processorImpl('expr)}

def processorImpl(expr: Expr[Any])(using Quotes): Expr[AST.Tree] =
  import quotes.reflect.*

  object transform extends TreeMap {
    override def transformStats(ls: List[Statement])(owner: Symbol): List[Statement] = 
      ls.map {
        case t: Term => transformTerm(t)(owner)

        case ValDef(name, tt, Some(init)) => 
          if(tt.tpe =:= TypeRepr.of[Value]) then
            init match {
              case Ident("uninitialized") => '{AST.Define(${Expr(name)}, None)}.asTerm
              case _ => '{ 
                AST.Define(${Expr(name)}, Some(${transformTerm(init)(owner).asExprOf[AST.Tree]}))
              }.asTerm
            }
          else throw RuntimeException(s"Unsupported type: ${tt.tpe.show} (expected: ${TypeRepr.of[Value].show})")

        case s => throw RuntimeException(s"Unsupported statement: ${s.getClass}")
      }

    override def transformTerm(t: Term)(owner: Symbol): Term = t match 
      //case Literal(UnitConstant()) => Expr(Nil).asTerm

      case Ident("inbox") => '{ AST.Inbox }.asTerm
      case Ident("outbox") => throw RuntimeException("Cannot access outbox value")
      case Ident("uninitialized") => throw RuntimeException("Cannot access uninitialized value")
      case Ident(varName) => '{ AST.Variable(${Expr(varName)}) }.asTerm

      case Assign(Ident("outbox"), expr) => 
        '{ AST.Outbox(${transformTerm(expr)(owner).asExprOf[AST.Tree]}) }.asTerm
      case Assign(Ident(varName), expr) =>
        '{ AST.Assign(${Expr(varName)}, ${transformTerm(expr)(owner).asExprOf[AST.Tree]}) }.asTerm


      case Apply(Select(Ident(varName), methodName), List(Literal(IntConstant(1)))) => {
        methodName match 
          case "+=" => '{ AST.BumpUp(${Expr(varName)}) }
          case "-=" => '{ AST.BumpDown(${Expr(varName)}) }
          case _ => throw RuntimeException(s"Unsupported operation on Value: ${methodName}")
        }.asTerm

      case Apply(Select(lhs, methodName), List(Ident(rhsName))) => {
        methodName match 
          case "+" => '{ AST.Add(${transformTerm(lhs)(owner).asExprOf[AST.Tree]}, ${Expr(rhsName)}) }
          case "-" => '{ AST.Sub(${transformTerm(lhs)(owner).asExprOf[AST.Tree]}, ${Expr(rhsName)}) }
          case _ => throw RuntimeException(s"Unsupported operation on Value: ${methodName}")
        }.asTerm

      case Block(statements, expr) => 
        val tStats = Expr.ofList(transformStats(statements)(owner).map(_.asExprOf[AST.Tree]))
        val tExpr = transformTerm(expr)(owner).asExprOf[AST.Tree]
        '{
          AST.Sequence(${tStats} :+ ${tExpr})
        }.asTerm

      case While(Literal(BooleanConstant(true)), body) => 
        val tBody = transformTerm(body)(owner)
        '{ 
          AST.Loop(${tBody.asExprOf[AST.Tree]})
        }.asTerm

      case Inlined(_, _, code) => 
        transformTerm(code)(owner)

      case _ => throw RuntimeException(s"Unsupported construct: ${Printer.TreeStructure.show(t)}")
  }

  transform.transformTerm(expr.asTerm)(Symbol.spliceOwner).asExprOf[AST.Tree]
