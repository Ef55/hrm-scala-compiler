import hrsm.*

import java.io.*

object Year {
  val outPath = File(s"examples/output/")

  inline def apply(year: Int)(inline expr: Any): Unit = 
    val program = hrassembly(expr)
    println(print(program))
    outPath.mkdirs
    write(File(outPath, s"year${year}.asm"), program)
}