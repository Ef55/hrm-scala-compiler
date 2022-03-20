import hrsm.Language.*
import hrsm.hrprocessor.given

// Statement: Output the input multiplied by 40
// Solve both optimization challenges

@main
def year12: Unit =
  Year(12)(5){
    while True do
      var r: Variable[Int] = inbox
      r = r + r // 2
      r = r + r // 4
      var i8: Variable[Int] = r + r 
      r = i8 + i8 // 16
      outbox = r + r + i8
  }
