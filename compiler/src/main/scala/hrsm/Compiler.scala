package hrsm

import Arguments.*
import Language.*
import hrsm.{MachineCode as MC}

import scala.collection.mutable.Map

def compile(code: AST.Tree): MC.Program = 

  var lastLabel = -1
  def freshLabel: Identifier =
    lastLabel += 1
    s"l${lastLabel}"

  val variables = Map.empty[String, ConcreteValue]
  var lastVar = -1
  def defineVariable(name: String): ConcreteValue =
    lastVar += 1
    variables += (name -> ConcreteValue(lastVar))
    getVar(name)

  def getVar(name: String): ConcreteValue = variables(name)



  def rec(code: AST.Tree): MC.Program = code match
    case AST.Sequence(nodes) => nodes.flatMap(rec(_))
    case AST.Define(id, init) => 
      val nv = defineVariable(id)
      init match {
        case None => List()
        case Some(expr) => rec(expr) :+ MC.CopyTo(Immediate(nv))
      }
    case AST.Assign(id, expr) => rec(expr) :+ MC.CopyTo(Immediate(getVar(id)))
    case AST.Inbox => List(MC.Inbox)
    case AST.Outbox(expr) => rec(expr) :+ MC.Outbox
    case AST.Variable(id) => List(MC.CopyFrom(Immediate(getVar(id))))
    case AST.Add(lhs, rhs) => 
      rec(lhs) :+ MC.Add(Immediate(getVar(rhs)))
    case AST.Sub(lhs, rhs) => 
      rec(lhs) :+ MC.Sub(Immediate(getVar(rhs)))
    case AST.BumpUp(id) => List(MC.BumpUp(Immediate(getVar(id))))
    case AST.BumpDown(id) => List(MC.BumpDown(Immediate(getVar(id))))
    case AST.Loop(body) => 
      val lbl = freshLabel
      MC.Label(lbl) +: rec(body) :+ MC.Jump(lbl)


  rec(code)
