package hrsm

import hrsm.{Language as AST}
import hrsm.{MachineCode as MC}

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{SortedSet, Map}
import hrsm.Language.Uninitialized

def compile(code: AST.Tree[Any])(config: Configuration): MC.Program = 
  import AST.Literal
  import AST.{Immediate, Indirect}

  def createConstantsMap(mem: IMap[Int, Int]): Map[Literal, Literal] = 
    val r = Map.empty[Literal, Literal]
    config.init.foreach{(k, v) =>
      r += (Literal(v) -> Literal(k))
    }
    r
  val constants = createConstantsMap(config.init)
  // println(config.init)
  // println(constants)

  var lastLabel = -1
  def freshLabel: String =
    lastLabel += 1
    s"l${lastLabel}"

  var freeMemory = SortedSet.from[Int](0 until config.memorySize) &~ config.init.keySet
  def memorySnapshot[T](expr: => T): T =
    val snapshot = SortedSet.from(freeMemory)
    val r = expr
    freeMemory = snapshot
    r

  val variables = Map.empty[Identifier, Literal]
  def defineVariable(name: Identifier): Literal =
    val newVar = freeMemory.head
    freeMemory -= newVar
    variables += (name -> Literal(newVar))
    getVar(name)

  def getVar(name: Identifier): Literal = variables(name)

  var register: Option[Literal] = None


  def getJump(cmp: AST.Comparator): String => MC.Instruction = 
    cmp match 
      case AST.Comparator.Eq | AST.Comparator.Neq => MC.JumpZ(_)
      case AST.Comparator.Less | AST.Comparator.Geq => MC.JumpN(_)

  def rec(code: AST.Tree[Any]): MC.Program = code match
    case AST.Nop => List()

    case AST.Sequence(nodes, last) => 
      memorySnapshot{
        (nodes :+ last).flatMap(rec(_))
      }

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

    case AST.UserVariable(id) => 
      val loc = getVar(id)
      if register == Some(loc) then List()
      else List(MC.CopyFrom(Immediate(loc)))

    case _: AST.Variable[Any] =>
      throw Exception(s"Invalid use of built-in variable: ${code}")

    case lit: AST.Literal =>
      constants.get(lit) match 
        case Some(i) => List(MC.CopyFrom(Immediate(i)))
        case None => throw Exception(s"Unavailable constant: ${lit.i}")

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

    case AST.Ite(cond, thenn, None) => cond match 
      case AST.Cond(cond, comp@(AST.Comparator.Neq | AST.Comparator.Geq)) => 
        val jump = getJump(comp)
        val mem = freeMemory
        val end = freshLabel
        val r = memorySnapshot{
          rec(cond) ++ ( jump(end) +: rec(thenn) ) :+ MC.Label(end)
        }
        register = None
        r
      case AST.Cond(cond, comp@(AST.Comparator.Eq | AST.Comparator.Less)) => 
        val jump = getJump(comp)

        val start = freshLabel
        val end = freshLabel
        val r = memorySnapshot {
          ( rec(cond) :+ jump(start) :+ MC.Jump(end) :+ MC.Label(start) ) ++ rec(thenn) :+ MC.Label(end)
        }
        register = None
        r
      
    case AST.Ite(cond, thenn, Some(elze)) => cond match
      case AST.Cond(cond, comp@(AST.Comparator.Neq | AST.Comparator.Geq)) => 
        rec(AST.Ite(AST.Cond(cond, comp.invert), elze, Some(thenn)))
      case AST.Cond(cond, comp@(AST.Comparator.Eq | AST.Comparator.Less)) => 
        val jump = getJump(comp)

        val mid = freshLabel
        val end = freshLabel

        val condMc = memorySnapshot{
          rec(cond)
        }
        val regState = register

        val thenMc = memorySnapshot{
          ( jump(mid) +: rec(elze) :+ MC.Jump(end) )
        }
        register = regState

        val elseMc = memorySnapshot{
          ( MC.Label(mid) +: rec(thenn) :+ MC.Label(end) )
        }
        register = None
        condMc ++ thenMc ++ elseMc

    case AST.Uninitialized | AST.Cond(_, _) | AST.True => throw Exception(s"Invalid top-level boolean: ${code}")

  rec(code)
