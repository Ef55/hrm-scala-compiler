import hrsm.*
import Language.*
import hrprocessor.{*,given}
// Statement: Output the max of every pair of inputs

@main
def year14: Unit =
  Year(14)(3){
    While(True) {
      val f: Variable[Int] = ! inbox
      val s: Variable[Int] = ! inbox
      If(s < f){
        outbox =! f
      }{
        outbox =! s
      }
    }
  }