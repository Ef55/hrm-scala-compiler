package hrsm

inline def hrassembly(inline expr: Any): MachineCode.Program = compile(hrprocessor(expr))
