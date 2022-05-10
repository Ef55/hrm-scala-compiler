import hrsm.*
import Language.*
import hrprocessor.{*,given}
// Statement: Sort (and output) every 3-uple from the input

@main
def year28Sorting: Unit =
  Year(28)(10){
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