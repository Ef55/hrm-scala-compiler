package hrsm

import scala.collection.immutable.Map

case class Configuration(memorySize: Int, init: Map[Int, Int])

inline def hrassembly(inline expr: Any)(using config: Configuration): MachineCode.Program = compile(hrprocessor(expr))(config)
