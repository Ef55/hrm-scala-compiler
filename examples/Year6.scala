import hrsm.Language.*
import hrsm.hrprocessor.given

// Statement: Add every pair of inputs and output the result
// Solve both optimization challenges

@main
def year06: Unit =
  Year(6)(3){
    while True do
      val toAdd: Variable[Int] = inbox
      outbox = inbox + toAdd
  }
