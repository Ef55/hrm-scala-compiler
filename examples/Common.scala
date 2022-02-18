import hrsm.*

import hrsm.MachineCode.Label
import java.io.*
import scala.collection.immutable.Map

def memory(ls: (Int, Int)*) = Map.from[Int, Int](ls)


object Year {
  val outPath = File(s"examples/output/")

  inline def apply(year: Int)(memorySize: Int)(inline expr: Any): Unit = 
    apply(year)(memorySize, Map.empty)(expr)

  inline def apply(year: Int)(memorySize: Int, init: Map[Int, Int])(inline expr: Any): Unit = 
    val program = hrassembly(expr)(using Configuration(memorySize, init))
    println(s"Generated a program for year ${year}")
    println(s"Program size: ${program.filter(!_.isInstanceOf[Label]).length}")
    outPath.mkdirs
    write(File(outPath, s"year${year}.asm"), program)
}