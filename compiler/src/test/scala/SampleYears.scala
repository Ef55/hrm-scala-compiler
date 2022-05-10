import hrsm.*

import Language.*
import hrprocessor.{*,given}
import MachineCode.Program

case class Year private (
  name: Int, 
  memorySize: Int, 
  init: Map[Int, Int], 
  code: Program
)

object Year {
  inline def apply(name: Int)(memorySize: Int)(inline expr: Language.Tree[Any]): Year = 
    Year(name)(memorySize, Map.empty)(expr)

  inline def apply(name: Int)(memorySize: Int, init: Map[Int, Int])(inline expr: Language.Tree[Any]): Year = {
    val program = hrassembly(expr)(using Configuration(memorySize, init))
    Year(name, memorySize, init, program)
  }
}

val year2 = Year(2)(0){
    While(True) {
      outbox =! inbox
    }
  }

val year6 = Year(6)(3){
    While(True){
      val toAdd: Variable[Int] = ! inbox
      outbox =! inbox + toAdd
    }
  }

val year7 = Year(7)(9){
    While(True){
      val in: Variable[Int] = ! inbox
      If(in !== 0){ outbox =! in }
    }
  }

val year9 = Year(9)(9){
    While(True){
      val in: Variable[Int] = ! inbox
      If(in === 0){ outbox =! in }
    }
  }

val year12 = Year(12)(5){
    While(True){
      var r: Variable[Int] = ! inbox
      r =! r + r // 2
      r =! r + r // 4
      var i8: Variable[Int] = ! (r + r)
      r =! i8 + i8 // 16
      outbox =! r + r + i8
    }
  }

val year17 = Year(17)(5, memory(4 -> 0, 5 -> 1)){
    While(True){
      If (inbox < 0) {
        If (inbox < 0) {
          outbox =! 0
        }{
          outbox =! 1
        }
      }{
        If (inbox < 0) {
          outbox =! 1
        }{
          outbox =! 0
        }
      }
    }
  }

val year28 = Year(28)(10){
    While(True){
      var fst: Variable[Int] = ! inbox
      var snd: Variable[Int] = ! inbox
      var trd: Variable[Int] = ! inbox

      If(snd < fst){
        val tmp = ! fst
        fst =! snd
        snd =! tmp
      }
      If(trd < snd){
        val tmp = ! trd
        trd =! snd
        snd =! tmp
        If(snd < fst){
          val tmp = ! fst
          fst =! snd
          snd =! tmp
        }
      }
      outbox =! fst
      outbox =! snd
      outbox =! trd
    }
  }