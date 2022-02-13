import hrsm.Language.*

// Statement: Add every pair of inputs and output the result
// Solve both optimization challenges

@main
def year06: Unit =
  Year(6){
    var toAdd = uninitialized
    while true do
      toAdd = inbox
      outbox = inbox + toAdd
  }
