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

  var register: Option[ConcreteValue] = None

  def rec(code: AST.Tree): MC.Program = code match
    case AST.Sequence(nodes) => nodes.flatMap(rec(_))
    case AST.Define(id, init) => 
      val nv = defineVariable(id)
      init match
        case None => List()
        case Some(expr) => rec(AST.Assign(id, expr))
    case AST.Assign(id, expr) => 
      val loc = getVar(id)
      val r = rec(expr) :+ MC.CopyTo(Immediate(loc))
      register = Some(loc)
      r
    case AST.Inbox => 
      register = None
      List(MC.Inbox)
    case AST.Outbox(expr) => 
      val r = rec(expr) :+ MC.Outbox
      register = None
      r
    case AST.Variable(id) => 
      val loc = getVar(id)
      if register == Some(loc) then List()
      else List(MC.CopyFrom(Immediate(loc)))
    case AST.Add(lhs, rhs) => 
      val r = rec(lhs) :+ MC.Add(Immediate(getVar(rhs)))
      register = None
      r
    case AST.Sub(lhs, rhs) => 
      val r = rec(lhs) :+ MC.Sub(Immediate(getVar(rhs)))
      register = None
      r
    case AST.BumpUp(id) => 
      val loc = getVar(id)
      register = Some(loc)
      List(MC.BumpUp(Immediate(loc)))
    case AST.BumpDown(id) => 
      val loc = getVar(id)
      register = Some(loc)
      List(MC.BumpDown(Immediate(loc)))
    case AST.Loop(body) => 
      val lbl = freshLabel
      MC.Label(lbl) +: rec(body) :+ MC.Jump(lbl)


  rec(code)
