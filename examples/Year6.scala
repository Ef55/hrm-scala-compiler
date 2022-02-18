import hrsm.Language.*

// Statement: Add every pair of inputs and output the result
// Solve both optimization challenges

@main
def year06: Unit =
  Year(6)(3){
    while true do
      val toAdd = inbox
      outbox = inbox + toAdd
  }
