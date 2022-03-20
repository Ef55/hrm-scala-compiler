package hrsm

import scala.collection.immutable.Map
import scala.compiletime.codeOf

case class Configuration(memorySize: Int, init: Map[Int, Int])

def memory(ls: (Int, Int)*) = Map.from[Int, Int](ls)

inline def hrassembly(inline expr: Language.Tree[Any])(using config: Configuration): MachineCode.Program =
  compile(hrprocessor(expr))(config)
