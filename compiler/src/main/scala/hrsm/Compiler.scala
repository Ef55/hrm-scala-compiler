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

  def getJump(cmp: Comparator): Identifier => MC.Instruction = 
    cmp match 
      case Comparator.Eq | Comparator.Neq => MC.JumpZ(_)
      case Comparator.Less | Comparator.Geq => MC.JumpN(_)

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

    case AST.Ite(cond, comp, thenn, None) => comp match 
      case Comparator.Neq | Comparator.Geq => 
        val jump = getJump(comp)

        val end = freshLabel
        val r = rec(cond) ++ ( jump(end) +: rec(thenn) ) :+ MC.Label(end)
        register = None
        r
      case Comparator.Eq | Comparator.Less => 
        val jump = getJump(comp)

        val start = freshLabel
        val end = freshLabel
        val r = ( rec(cond) :+ jump(start) :+ MC.Jump(end) :+ MC.Label(start) ) ++ rec(thenn) :+ MC.Label(end)
        register = None
        r
      
    case AST.Ite(cond, comp, thenn, Some(elze)) => comp match
      case Comparator.Neq | Comparator.Geq => rec(AST.Ite(cond, comp.invert, elze, Some(thenn)))
      case Comparator.Eq | Comparator.Less => 
        val jump = getJump(comp)

        val mid = freshLabel
        val end = freshLabel

        val condMc = rec(cond)
        val regState = register
        val thenMc = ( jump(mid) +: rec(elze) :+ MC.Jump(end) )
        register = regState
        val elseMc = ( MC.Label(mid) +: rec(thenn) :+ MC.Label(end) )
        register = None
        condMc ++ thenMc ++ elseMc


  rec(code)
