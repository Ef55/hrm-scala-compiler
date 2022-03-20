package hrsm

import scala.collection.immutable.Map
import scala.compiletime.codeOf

case class Configuration(memorySize: Int, init: Map[Int, Int])

inline def hrassembly(inline expr: Language.Tree[Any])(using config: Configuration): MachineCode.Program =
  compile(hrprocessor(expr))(config)
