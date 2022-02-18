package hrsm

case class Configuration(memorySize: Int)

inline def hrassembly(inline expr: Any)(using config: Configuration): MachineCode.Program = compile(hrprocessor(expr))(config.memorySize)
