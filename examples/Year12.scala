import hrsm.Language.*

// Statement: Output the input multiplied by 40
// Solve both optimization challenges

@main
def year12: Unit =
  Year(12){
    var r = uninitialized
    var i8 = uninitialized
    while true do
      r = inbox
      r = r + r // 2
      r = r + r // 4
      i8 = r + r 
      r = i8 + i8 // 16
      outbox = r + r + i8
  }
