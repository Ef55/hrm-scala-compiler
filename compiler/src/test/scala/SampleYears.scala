import hrsm.*

import Language.{*, given}
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
    while True do
      outbox = inbox
  }

val year6 = Year(6)(3){
    while True do
      val toAdd: Variable[Int] = inbox
      outbox = inbox + toAdd
  }

val year7 = Year(7)(9){
    while True do
      val in: Variable[Int] = inbox
      if(in !== 0) then outbox = in
  }

val year9 = Year(9)(9){
    while True do
      val in: Variable[Int] = inbox
      if(in === 0) then outbox = in
  }

val year12 = Year(12)(5){
    while True do
      var r: Variable[Int] = inbox
      r = r + r // 2
      r = r + r // 4
      var i8: Variable[Int] = r + r 
      r = i8 + i8 // 16
      outbox = r + r + i8
  }

val year17 = Year(17)(5, memory(4 -> 0, 5 -> 1)){
    while True do
      if inbox < 0 then
        if inbox < 0 then
          outbox = 0
        else
          outbox = 1
      else
        if inbox < 0 then
          outbox = 1
        else
          outbox = 0
  }

val year28 = Year(28)(10){
    while True do
      var fst: Variable[Int] = inbox
      var snd: Variable[Int] = inbox
      var trd: Variable[Int] = inbox

      if snd < fst then
        val tmp = fst
        fst = snd
        snd = tmp

      if trd < snd then
        val tmp = trd
        trd = snd
        snd = tmp
        if snd < fst then
          val tmp = fst
          fst = snd
          snd = tmp
      
      outbox = fst
      outbox = snd
      outbox = trd
  }